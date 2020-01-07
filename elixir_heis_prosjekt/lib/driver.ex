defmodule Driver do
  use Supervisor

  def start_link(init_args) do
    Supervisor.start_link(__MODULE__, init_args, name: __MODULE__)
  end

  defmodule SetMotorDir do
    def up(),    do: RawDriver.set_motor_dir(:motor_up)
    def down(),  do: RawDriver.set_motor_dir(:motor_down)
    def still(), do: RawDriver.set_motor_dir(:motor_still)


  end

  defmodule SetLight do
    def button(floor, dir, wanted_state) do
      RawDriver.set_button_light(floor, dir, wanted_state)
    end
    def floor_indicator(floor) do
      RawDriver.set_floor_indicator(floor)
    end
    def door(state) do
      RawDriver.set_door_state(state)
    end
  end
  def init(init_args) do
    children = [
      # FloorSubscriber,
      {RawDriver, init_args},
      {FloorSubscriber, init_args}
      | ButtonSubscriber.get_subscribers_specs(init_args)
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

end

# Intermal modules needed for the driver


defmodule ButtonSubscriber do
  # TODO Implement this

  def get_subscribers_specs(args) do
    button_types = get_all_buttons(args.n_floors)
    get_spec = fn(button_type)-> child_spec({button_type, args}) end
    Enum.map(button_types,get_spec )
  end
  def child_spec({{ floor, dir}, args}) do
    mod_str = Atom.to_string __MODULE__
    floor_str = Integer.to_string floor
    dir_str = Atom.to_string dir
    name_str = mod_str <> "_" <> floor_str <>"_"<> dir_str
    name = String.to_atom name_str
    %{
      id: name,
      start: {__MODULE__, :start_link, [{ floor, dir}]}
    }
  end

  @spec get_all_buttons(integer) :: [any]
  def get_all_buttons(n_floors) do
    dirs = [:hall_up, :hall_down, :cab]
    all_combinations = for floor <- 1..n_floors, dir <- dirs, do: {floor, dir}
    all_combinations --[{n_floors, :hall_up}, {1, :hall_down}]
  end

  def start_link({floor, dir}) do
    pid = spawn_link(__MODULE__, :main, [{floor, dir}])
    {:ok, pid} #All children are linked, so only one is needed for the restart(?)
  end

  def main({floor, dir}) do
    :timer.sleep(100)
    is_button_pressed = RawDriver.poll_button?(floor, dir)

    # Holding the button down on the simulator is read as fast tapping
    if is_button_pressed   do
      IO.write "Pressed: Button #{floor}"
      IO.inspect dir
      IO.inspect is_button_pressed
      #TODO Handle subscription
    end
    main({floor, dir})
  end

end

defmodule FloorSubscriber do
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args.floor_subscriber_funs]}
    }
  end

  def start_link(subscriber_funs \\ []) do
    pid = spawn_link(__MODULE__, :main, [subscriber_funs])
    {:ok, pid}
  end

  def main(subscriber_funs, prev_state\\:between_floors) do
    :timer.sleep(100)
    floor_state = RawDriver.poll_floor()

    unless floor_state == :between_floors do
      unless prev_state == floor_state do

        for foo<-subscriber_funs, do: foo.(floor_state)
      end
    end

    # IO.inspect(floor_state)
    main(subscriber_funs, floor_state)
  end
end



defmodule RawDriver do
  use GenServer
  @type button_dir :: :hall_up | :hall_down | :cab

  # TODO Implement calls to GenServer
  def child_spec(args) do
    ip = args.ip
    port = args.port

    %{
      id: RawDriver,
      start: {RawDriver, :start_link, [{ip, port}]}
    }
  end

  def start_link({ip, port}) do
    result = GenServer.start_link(__MODULE__, {ip, port}, name: __MODULE__)
    IO.write("Gen server was started")
    IO.inspect(result)
    result
  end

  @impl true
  def init({ip, port}) do
    result = :gen_tcp.connect(ip, port, [{:active, false}])
    IO.inspect(result)
    result
  end

  def button_dir?(dir) do
    Enum.member?([:hall_up, :hall_down, :cab], dir)
  end

  @spec poll_floor :: integer() | :between_floors
  def poll_floor() do
    GenServer.call(__MODULE__, :poll_floor_state)
  end

  @spec poll_button?(integer, button_dir) :: boolean
  def poll_button?(floor, dir) do
    case button_dir?(dir) do
      true -> GenServer.call(__MODULE__, {:poll_button, floor, dir}, :infinity)
      _ -> {:error, :invalid_dir, dir}
    end
  end
  def set_motor_dir(motor_dir) do
    motor_dirs = [:motor_up, :motor_down, :motor_still]
    case Enum.member?(motor_dirs, motor_dir) do
      true-> GenServer.cast(__MODULE__, {:set_motor_dir, motor_dir});
      _-> :invalid_input
    end
  end
  def set_button_light(floor, dir, wanted_state) do
    is_state = Enum.member?( [:on, :off], wanted_state)
    is_dir = button_dir?(dir)
    case is_state and is_dir do
      true -> GenServer.cast(__MODULE__, {:set_button_light, floor, dir, wanted_state})
      _->
        IO.write "Got invalid input"
        IO.inspect {floor, dir, wanted_state}
        :invalid_input
    end
  end
  def set_floor_indicator(floor) do
    GenServer.cast(__MODULE__, {:set_floor_indicator, floor})
  end
  # Internally needed functions
  def set_door_state(door_state) do
    case door_state do
      :open -> GenServer.cast(__MODULE__, {:set_door_state, door_state})
      :closed ->GenServer.cast(__MODULE__, {:set_door_state, door_state})
      _-> :invalid_input
    end

  end
  @impl true
  def handle_cast({:set_door_state, door_state}, socket) do
    door_code = %{open: 1, closed: 0}[door_state]
    :gen_tcp.send(socket, [4,door_code,0,0])
    {:noreply, socket}
  end
  def handle_cast({:set_motor_dir, motor_dir}, socket)do
    motor_codes = %{motor_up: 1, motor_down: 255, motor_still: 0 }
    motor_code = motor_codes[motor_dir]
    :gen_tcp.send(socket, [1, motor_code, 0,0])
    {:noreply, socket}
  end
  def handle_cast({:set_button_light, floor, dir, wanted_state}, socket) do
    state_codes_rec = %{on: 1, off: 0}
    dir_codes_rec = %{hall_up: 0, hall_down: 1, cab: 2}
    state_code = state_codes_rec[wanted_state]
    dir_code = dir_codes_rec[dir]
    message = [2, dir_code, floor, state_code]
    :gen_tcp.send(socket, message)
    {:noreply, socket}
  end
  def handle_cast({:set_floor_indicator, floor}, socket) do
    :gen_tcp.send( socket, [3,floor,0,0] )
    {:noreply, socket}
  end
  def handle_cast(invalid_message, socket) do
    IO.write "Raw-driver got invalid message: "
    IO.inspect invalid_message
    {:noreply, socket}
  end


  # Handle calls
  @impl true
  def handle_call(:poll_floor_state, _from, socket) do
    :gen_tcp.send(socket, [7, 0, 0, 0])
    # IO.puts("Polling")

    state =
      case :gen_tcp.recv(socket, 4, 1000) do
        {:ok, [7, 0, _, 0]} -> :between_floors
        {:ok, [7, 1, floor, 0]} -> floor
      end

    {:reply, state, socket}
  end
  def handle_call({:poll_button, floor, dir}, _from, socket) do
    # TODO Define types
    dir_codes = %{hall_up: 0, hall_down: 1, cab: 2}
    dir_code = dir_codes[dir]
    message = [6, dir_code, floor-1, 0] #TODO Fix the 0-indexing of the floors
    :gen_tcp.send(socket, message)
    result = :gen_tcp.recv(socket, 4, 1000)
    {:ok, response} = result

    server_reply =
      case response do
        [6, button_state, 0, 0] -> button_state === 1
        _ -> :error_wrong_reply
      end

    {:reply, server_reply, socket}
  end
  def handle_call(request, from, socket) do
    IO.write("Unknown call: ")
    IO.inspect(request)
    IO.inspect(from)
    {:reply, {:error, "Unknown call"}, socket}
  end
end
