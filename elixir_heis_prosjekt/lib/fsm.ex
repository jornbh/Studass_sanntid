defmodule FiniteStateMachine do
  use GenServer
  # API
  ######################################################################
  @doc "Must contain n_floors"
  def start_link(args) do
    GenServer.start_link(__MODULE__, [args], name: __MODULE__)
    # pid = spawn_link(__MODULE__, :main, [args])
    # {:ok, pid}
  end

  def give_destination(floor) do
    GenServer.cast(__MODULE__, {:give_new_destination, floor})
  end
  def arrive_floor( floor ) do
    GenServer.cast(__MODULE__,{:arrived_at_floor, floor} )
  end
  def say_timeout(state) do
    GenServer.cast(__MODULE__,{:timeout, state} )
  end
  # Internal functions
  ######################################################################
  def init(_args)  do
    Driver.SetMotorDir.down
    # floor = receive do
    #   {:floor_sensor, floor} -> floor
    # end
    # Driver.SetMotorDir.still()
    # Driver.SetLight.floor_indicator(floor)
    # IO.write "Init complete"
    # IO.inspect floor

    FSMWatchdog.start_link()
    state = :running
    sub_state = %{floor: -1, destination: :none, dir: :up}
    {:ok, {state, sub_state}}
  end


  # States = waiting, running, open
  def handle_cast( message, {state, sub_state} ) do
    IO.puts "Main"
    IO.inspect message
    event = process_message( message, state, sub_state  )
    {new_state, new_sub_state} = get_next_state(event, state, sub_state)
    IO.inspect event
    IO.inspect {new_state, new_sub_state}


    leave_commands = leave_state(state, new_state, new_sub_state)
    event_commands = handle_event( event, new_state, new_sub_state )
    enter_commands = enter_state(state, new_state, new_sub_state)

    run_commands( leave_commands++ event_commands ++ enter_commands )
    {:noreply, {new_state, new_sub_state}}
  end

  def handle_call(message, from, total_state) do
    IO.write "FSM got invalid call"
    IO.inspect {message, from, total_state}
  end
  def process_message( {:arrived_at_floor, floor}, _state, sub_state ) do
    cond  do
      floor == sub_state.floor -> :ignore_message
      floor != sub_state.floor -> {:arrived_at_floor, floor }
      true                     -> :error
    end
  end

  # All unprocessed messages
  def process_message( message, state, _sub_state ) do
    case message do
      {:give_new_destination, _floor} -> message
      {:timeout, ^state} -> message
      _->
        IO.write "Unknown message"
        IO.inspect message
        :implement_this
    end
  end

  def get_next_state( {:give_new_destination, floor}, _state, sub_state=%{floor: floor} ) do
    {:open, %{sub_state | destination: floor}}
  end
  def get_next_state( {:give_new_destination, floor}, state, sub_state ) do

    dir_to_order = cond do
      floor > sub_state.floor ->:up
      floor < sub_state.floor ->:down
      true -> :still
    end
    new_dir = cond do
      state == :running-> sub_state.dir
      state == :waiting-> dir_to_order
      state == :open   -> dir_to_order
    end

    new_state = case state do
      :waiting -> :running
      _ -> state
    end
    new_sub_state = %{sub_state| destination: floor, dir: new_dir}
    {new_state, new_sub_state}
  end
  def get_next_state({:arrived_at_floor, floor }, _state, sub_state) do
    destination = sub_state.destination
    new_state = cond  do
      destination == floor -> :open
      destination == :none -> :waiting
      true -> :running
    end
    new_dir = case new_state do
      :waiting -> :still
      _-> sub_state.dir
    end
    new_dest = if (new_state == :open), do: :none, else: sub_state.destination
    new_sub_state =%{ sub_state | dir: new_dir, floor: floor, destination: new_dest}
    IO.inspect {new_state, new_sub_state}
    {new_state, new_sub_state}
  end
  def get_next_state({:timeout, :open}, :open, sub_state) do
    #TODO implement this
    destination = sub_state.destination
    floor = sub_state.floor
    new_state = cond do
      destination == :none -> :waiting
      destination == floor -> :open
      true -> :running
    end

    new_dir = cond do
      destination == :none -> :still
      destination > floor -> :up
      destination < floor -> :down
    end
    new_sub_state = %{sub_state | dir: new_dir}
    {new_state, new_sub_state}
  end
  def get_next_state(_event, state, sub_state) do # When no change
    {state, sub_state}
  end


  def enter_state(same_state, same_state, _new_sub_state) do
    [] # No transition -> No actions
  end
  def enter_state(_old_state, :running, new_sub_state) do
    motor_dir = cond  do
      new_sub_state.floor > new_sub_state.destination -> :down
      new_sub_state.floor < new_sub_state.destination -> :up
      true -> Process.exit(self(), "In state running, when dest == floor") # Something is wrong
    end
    [{Driver.SetMotorDir, motor_dir, []}] # No transition -> No actions
  end
  def enter_state(_old_state, :open, _new_sub_state) do
    [
      {Driver.SetLight, :door, [:open]},
      {FSMWatchdog, :set_timeout, [:open, 4000]}
    ]
  end
  def enter_state(_old_state, new_state, _new_sub_state) do
    [{FSMWatchdog, :set_timeout, [new_state, :infinity]}] # Ignore transition
  end

  def leave_state(same_state, same_state, _new_sub_state) do
    [] # No transition -> No actions
  end
  def leave_state(:running, _new_state , _new_sub_state) do
    [{Driver.SetMotorDir,:still, []} ]
  end
  def leave_state(:open,_new_state, _new_sub_state) do
    [{Driver.SetLight, :door, [:closed]}]
  end
  def leave_state(_old_state, _new_state, _new_sub_state) do
    [] # Ignore transition
  end

  def handle_event( {:arrived_at_floor, floor}, _state, _sub_state)do
    [{Driver.SetLight,:floor_indicator, [floor]}] #TODO add to update queue...
  end
  #TODO add a bump-function
  def handle_event( _event, _state, _sub_state)do
    [] # Ignore event
  end

  def run_commands(commands_list) do
    apply_fun = fn({module, function, args})-> apply( module, function, args ) end
    Enum.each( commands_list, apply_fun ) # Sends all constructed messages
  end
end


defmodule FSMWatchdog do
  def start_link() do
    pid = spawn_link( __MODULE__, :main, [] )
    {:ok, pid}
  end

  def main() do
    Process.register(self(), __MODULE__)
    main_loop( :no_state, :infinity)
  end
  def set_timeout( state, timeout ) do # Set timeout for a state
    send(  __MODULE__, {state, timeout})
  end
  def main_loop(state, timeout) do
    {new_state, new_timeout} = receive do
       message -> message
    after
      timeout ->
        FiniteStateMachine.say_timeout(state)
        {state, :infinity}
    end
    main_loop(new_state, new_timeout )
  end
end
