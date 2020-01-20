
defmodule RawDriver do
  @moduledoc"""
  All credits to Jostein for implementing this
  ## Description
  You must start the driver with `start_link()` or `start_link(ip_address, port)` before any of the other functions will work

  ## API:
  ```
  {:ok, driver_pid} = Driver.start_link
  set_motor_direction( driver_pid, motor_direction  )
  set_order_button_light( driver_pid, button_direction ,floor, on_or_off   )
  set_floor_indicator( driver_pid, floor )
  set_stop_button_light( driver_pid, on_or_off )
  set_door_open_light( driver_pid, on_or_off )
  get_order_button_state( driver_pid,floor, button_direction   )
  get_floor_sensor_state( driver_pid )
  get_stop_button_state( driver_pid )
  get_obstruction_switch_state( driver_pid )
  ```

  ## Further reading
  GenServers are a really neat way to make servers without having to rewrite the same code all the time. It works *Exactly* the same in erlang as well, but it is called gen_server instead. The erlang documentation is kind of hard understand, so use the elixir-video and "Translate" it to erlang (gen_server:call(...) instead of GenServer.call(...)).

  Short version is that a GenServer implements the basic parts of a server, and the code seen in this file is the "Blanks you have to fill in"

  ### A youtube-video that explains GenServers and Supervisors
  https://www.youtube.com/watch?v=3EjRvaCOl94

  """
  use GenServer
  @type button_dir :: :hall_up | :hall_down | :cab
  # Define Types used by dialyzer
  @type button :: :hall_up | :hall_down | :cab
  @type motor :: :up | :down | :stop
  @type state :: :on | :off
  @type ip_address :: {integer(), integer(), integer(), integer()}

  def child_spec(ip, port) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [ip, port]}
    }
  end

  def start_link() do
    start_link({127,0,0,1}, 15657)
  end

  def start_link(ip, port) do
    GenServer.start_link(__MODULE__, {ip, port}, name: __MODULE__)
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
