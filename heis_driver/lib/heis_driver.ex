defmodule Driver do
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
  @call_timeout 1000
  @button_map %{:hall_up => 0, :hall_down => 1, :cab => 2}
  @state_map  %{:on => 1, :off => 0}
  @direction_map %{:up => 1, :down => 255, :stop => 0}

  # Define Types used by dialyzer
  @type button :: :hall_up | :hall_down | :cab
  @type motor :: :up | :down | :stop
  @type state :: :on | :off
  @type ip_address :: {integer(), integer(), integer(), integer()}


  @doc"""
  Returns: `{:ok, driver_pid}` or `{:error, reason}`

  If error is retuned, the proces was not started properly
  """
  @spec start_link :: :ignore | {:error, any} | {:ok, pid}
  def start_link do
    start_link {127,0,0,1}, 15657
  end


  @doc"""
  Arguments: IP-address (tuple of 4 ints) and port (integer)
  Returns: `{:ok, driver_pid}` or `{:error, reason}`

  If error is retuned, the proces was not started properly
  """
  @spec start_link(ip_address, integer()) :: :ignore | {:error, any} | {:ok, pid}
  def start_link address, port do
    GenServer.start_link(__MODULE__, [address, port], [])
  end

  def stop pid do
    GenServer.stop pid
  end

  def init [address, port] do
    {:ok, socket} =:gen_tcp.connect(address, port, [{:active, false}])
    {:ok, socket}
  end


  # User API ----------------------------------------------
  # direction can be :up/:down/:stop
  @doc"""
  Arguments: driver_pid, (:up/:down/:stop)

  Returns nothing.

  ## Examples
  {:ok, pid} = Driver.start_link
  Driver.set_motor_direction( pid,:up  )
  """
  @spec set_motor_direction(pid, motor) :: :ok
  def set_motor_direction pid, direction do
    GenServer.cast pid, {:set_motor_direction, direction}
  end


  # button_type can be :hall_up/:hall_down/:cab
  # state can be :on/:off
  @doc"""
  Arguments: driver_pid, (:hall_up/:hall_down/:cab), (:up/:down/:stop), (:on/:off)

  Returns nothing.

  ## Examples
  {:ok, driver_pid} = Driver.start_link
  Driver.set_order_button_light( driver_pid, :hall_up,2, :on   )
  """
  @spec set_order_button_light(pid, button, integer, state) :: :ok
  def set_order_button_light pid, button_type, floor, state do
    GenServer.cast pid, {:set_order_button_light, button_type, floor, state}
  end


  @doc"""
  Arguments: driver_pid, (0/1/.../number_of_floors -1)

  Returns nothing.

  ## Examples
  {:ok, driver_pid} = Driver.start_link

  set_floor_indicator(driver_pid, 2)
  """
  @spec set_floor_indicator(pid, integer()) :: :ok
  def set_floor_indicator pid, floor do
    GenServer.cast pid, {:set_floor_indicator, floor}
  end

  # state can be :on/:off
  @doc"""
  Arguments: driver_pid, (:on/:off)

  Returns nothing.

  ## Examples
  {:ok, driver_pid} = Driver.start_link

  set_stop_button_light(driver_pid, :on)
  """
  @spec set_stop_button_light(pid, any) :: :ok
  def set_stop_button_light pid, state do
    GenServer.cast pid, {:set_stop_button_light, state}
  end

  @doc"""
  Arguments: driver_pid, (:on/:off)

  Returns nothing.

  ## Examples
  {:ok, driver_pid} = Driver.start_link

  set_door_open_light(driver_pid, :on)
  """
  @spec set_door_open_light(pid , state()) :: :ok
  def set_door_open_light pid, state do
    GenServer.cast pid, {:set_door_open_light, state}
  end

  @doc"""
  Arguments: driver_pid, (0/1/.../number_of_floors -1), (:hall_up/:hall_down/:cab)


  Returns (0/1)

  0 means not pressed


  ## Examples
  {:ok, driver_pid} = Driver.start_link

  Driver.get_order_button_state( driver_pid,1, :hall_up  )
  """
  @spec get_order_button_state(pid, integer(), button) :: any
  def get_order_button_state pid, floor, button_type do
    GenServer.call pid, {:get_order_button_state, floor, button_type}
  end

  @doc"""
  Arguments: driver_pid,

  Returns (1/2/.../top_floor or :between_floors )
  ## Examples
  {:ok, driver_pid} = Driver.start_link

  Driver.get_order_button_state( driver_pid,1, :hall_up  )

  """
  def get_floor_sensor_state pid do
    GenServer.call pid, :get_floor_sensor_state
  end

  @doc"""
  Arguments: driver_pid

  Returns (0/1 )

  0 means not pressed
  ## Examples
  {:ok, driver_pid} = Driver.start_link

  Driver.get_order_button_state( driver_pid,1, :hall_up  )

  """
  def get_stop_button_state pid do
    GenServer.call pid, :get_stop_button_state
  end
  @doc"""
  Arguments: driver_pid

  Returns (0/1 )

  0 means not obstructed
  ## Examples
  {:ok, driver_pid} = Driver.start_link

  Driver.get_stop_button_state( driver_pid )

  """
  def get_obstruction_switch_state pid do
    GenServer.call pid, :get_obstruction_switch_state
  end



  # Casts  ----------------------------------------------
  def handle_cast {:set_motor_direction, direction}, socket do
    :gen_tcp.send(socket, [1, @direction_map[direction], 0, 0])
    {:noreply, socket}
  end

  def handle_cast {:set_order_button_light, button_type, floor, state}, socket do
    :gen_tcp.send socket, [2, @button_map[button_type], floor, @state_map[state]]
    {:noreply, socket}
  end

  def handle_cast {:set_floor_indicator, floor}, socket do
    :gen_tcp.send socket, [3, floor, 0, 0]
    {:noreply, socket}
  end

  def handle_cast {:set_door_open_light, state}, socket do
    :gen_tcp.send socket, [4, @state_map[state], 0, 0]
    {:noreply, socket}
  end

  def handle_cast {:set_stop_button_light, state}, socket do
    :gen_tcp.send socket, [5, @state_map[state], 0, 0]
    {:noreply, socket}
  end



  # Calls  ----------------------------------------------
  def handle_call {:get_order_button_state, floor, order_type}, _from, socket do
    :gen_tcp.send socket, [6, @button_map[order_type], floor, 0]
    {:ok, [6, state, 0, 0]} = :gen_tcp.recv(socket, 4, @call_timeout)
    {:reply, state, socket}
  end


  def handle_call :get_floor_sensor_state, _from, socket do
    :gen_tcp.send socket, [7, 0, 0, 0]
    button_state = case :gen_tcp.recv(socket, 4, @call_timeout) do
      {:ok, [7, 0, _, 0]} -> :between_floors
      {:ok, [7, 1, floor, 0]} -> floor
    end
    {:reply, button_state, socket}
  end

  def handle_call :get_stop_button_state, _from, socket do
    :gen_tcp.send socket, [8, 0, 0, 0]
    button_state = case :gen_tcp.recv(socket, 4, @call_timeout) do
      {:ok, [8, 0, 0, 0]} -> :inactive
      {:ok, [8, 1, 0, 0]} -> :active
    end
    {:reply, button_state, socket}
  end

  def handle_call :get_obstruction_switch_state, _from, socket do
    :gen_tcp.send socket, [9, 0, 0, 0]
    button_state = case :gen_tcp.recv(socket, 4, @call_timeout) do
      {:ok, [9, 0, 0, 0]} -> :inactive
      {:ok, [9, 1, 0, 0]} -> :active
    end
    {:reply, button_state, socket}
  end
end
