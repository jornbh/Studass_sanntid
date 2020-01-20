defmodule QueueMaster do
  #TODO Fix this entire mess (All about cost, etc. )
  use GenServer

  # API
  ##################################################################
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]}
    }
  end
  def start_link(_args) do
    lazy_restart()
    GenServer.start_link( __MODULE__, [],name: {:global, __MODULE__} )
  end

  def claim_a_destination( node, floor, motor_dir ) do
    {_, floor, _} = claim_an_order(node, floor, motor_dir)
    floor
  end
  def claim_an_order(node, floor, motor_dir) do
    lazy_restart()
    command = {:claim_and_get_an_order, node, floor, motor_dir}
    GenServer.call({:global,__MODULE__}, command, 900000000)
  end

  def put_order(node, floor, button_dir) do
    lazy_restart()
    GenServer.cast({:global,__MODULE__}, {:put_order, node, floor, button_dir})
  end
  def get_world() do
    lazy_restart()
    GenServer.call({:global,__MODULE__}, :get_world)
  end

  def floor_served(node, floor, button_dir) do
    lazy_restart()
    GenServer.cast({:global,__MODULE__}, {:floor_served, node,floor, button_dir})
  end
  @impl true
  def handle_call(message, _from, _no_state) do
    # This module is just a wraper to determine a master
    response = GenServer.call( LocalOrderHandler, message )
    {:reply, response, :no_state }
  end

  @impl true
  def handle_cast(message, _no_state) do
    # This module is just a wraper to determine a master
    GenServer.cast( LocalOrderHandler, message )
    {:noreply, :no_state }
  end
  # Internal functions
  ##################################################################
  def lazy_restart() do
    # The master is stateless and is restarted when it is needed.
    # It does not matter if any of them are killed, as long as one survives
    case GenServer.whereis({:global, __MODULE__}) do
      :nil ->
        IO.puts "Master was dead, restarting it now"
        GenServer.start(__MODULE__, [], name: {:global, __MODULE__})
      _-> :ok
    end
  end
  @impl true
  def init(_args) do
    # The list of orders has to be up to date.
    # all_worlds = LocalOrderHandler.get_world
    {:ok, :empty_state}
  end
  @impl true
  def handle_call(message, _from, state) do
    IO.puts "Called master server directly. That is not suposed to happen"
    {:reply, message, state}
  end
end


defmodule LocalOrderHandler do
  @moduledoc"""
  world is a record
  reserved_orders: %{ node_id => {floor, dir}},
  free_orders: [{floor, dir}]
  """
  use GenServer
  # API
  ##################################################################
  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [args]}
    }
  end
  def start_link(args) do
    GenServer.start_link( __MODULE__, args,name:  __MODULE__ )
  end

  def put_order(node, floor, button_dir) do
    GenServer.cast(__MODULE__, {:put_order, node, floor, button_dir})
  end
  def get_world() do
    GenServer.call(__MODULE__, :get_world)
  end

  def floor_served(node, floor, button_dir) do
    GenServer.cast(__MODULE__, {:floor_served, node,floor, button_dir})
  end

  # Internal functions
  ##################################################################

  @impl true

  def init(_args) do
    world = %{reserved_orders: %{node() => :none,}, orders: [] }
    {:ok, world}
  end

  @impl true
  def handle_call(:get_world, _from, world)do
    {:reply, world, world}
  end

  def handle_call({:claim_and_get_an_order, e_node, floor, motor_dir}, _from, world) do
    reserved_orders = Map.values(world.reserved_orders)
    now = :erlang.monotonic_time()
    orders_in_progress = for {timeout, order}<-reserved_orders,
      timeout< now do
        order
    end
    free_orders = world.orders -- orders_in_progress
    get_cost = &(cost_function e_node, floor, motor_dir, &1, 4)
    cheapest_order = Enum.min_by free_orders, get_cost, fn()->:none end # atoms are allways larger than ints in erlang
    return_order = case cheapest_order do
      :none -> :none
      {o_node, _, :cab} when o_node != e_node -> :none
      _ -> cheapest_order
    end
    order_timeout = get_timeout(10)
    new_reserved_orders = Map.put(world.reserved_orders,e_node, {order_timeout, return_order})
    new_world = %{world | reserved_orders: new_reserved_orders}
    {:reply, return_order, new_world}
  end
  def handle_call( unknown_message, from, world ) do
    IO.puts "Got unknown message"
    IO.inspect unknown_message
    IO.inspect from
    {:reply, {:error, :unknown_message}, world}
  end
  @impl true
  def handle_cast({:put_order, e_node, floor, button_dir},  world) do
    IO.puts "Called"
    new_order = {e_node, floor, button_dir}
    new_free_orders = [new_order] ++ (world.orders -- [new_order])
    new_world = %{world| orders: new_free_orders}
    {:noreply, new_world}
  end

  def handle_cast({:floor_served, node, floor, motor_dir}, world) do
    button_dir = %{hall_up: :up, hall_down: :down, cab: nil}[motor_dir]
    new_free_orders = world.orders --[{node, floor, button_dir}] #! TODO Fix this
    reserving_nodes = Map.keys(world.reserved_orders)
    reserved_orders = Enum.map( reserving_nodes, &(Map.fetch!(world.reserved_orders, &1)) )
    swap_fun = fn
                ({^node, ^floor, :cab  }) -> :none
                ({_, ^floor, :hall_up  }) -> :none
                ({_, ^floor, :hall_down}) -> :none
                (order)-> order
              end
    new_reserved_orders = Enum.map( reserved_orders, swap_fun )
    ziped = Enum.zip(reserving_nodes, new_reserved_orders)
    new_reserved_orders_map = Enum.into(ziped, %{})
    new_world =%{world |
                    reserved_orders: new_reserved_orders_map,
                    orders: new_free_orders
                }
    {:noreply, new_world}
  end


  def get_timeout(seconds_after_now) do
    resolution = :erlang.system_info(:os_monotonic_time_source)[:resolution]
    now = :erlang.monotonic_time()
    now + resolution * seconds_after_now
  end

  def cost_function(e_node, _, _, {o_node,_,_}, _) when e_node != o_node do
    IO.puts "Cab from other node"
    :infinity # Infinite cost
  end
  def cost_function(e_node, e_floor, e_dir, order, n_floors) do
    IO.inspect order
    elev_dict = %{node: e_node, floor: e_floor, dir: e_dir}
    {o_node, o_floor, o_button_dir} = order
    o_dir = %{hall_up: :up, hall_down: :down, cab: :cab}[o_button_dir]
    order_dict = %{node: o_node, floor: o_floor, dir: o_dir}
    cost = simulator(elev_dict, order_dict, n_floors)
    cost
  end

  def simulator(elevator, order, n_floors, cost\\0)
  def simulator(elevator=%{dir: :still}, order, n_floors, cost) do
    new_dir = cond do
      order.floor > elevator.floor ->  :up
      order.floor < elevator.floor ->  :down
      true ->  order.dir
    end
    new_elev = %{elevator | dir: new_dir}
    simulator(new_elev, order, n_floors, cost)
  end
  def simulator(elevator=%{floor: n_floors, dir: :up}, order, n_floors, cost) do
    simulator(%{elevator| dir: :down}, order, n_floors, cost)
  end
  def simulator(elevator=%{floor: 1, dir: :down}, order, n_floors, cost) do
    simulator(%{elevator| dir: :up}, order, n_floors, cost)
  end
  def simulator(%{floor: floor, dir: dir},%{floor: floor, dir: dir}, _,cost) do
    cost
  end
  def simulator(%{floor: floor},%{floor: floor, dir: :cab}, _,cost) do
    cost
  end
  def simulator( elevator, order, n_floors, cost ) when cost <9000 do
    new_floor = case elevator do
      %{dir: :up} -> elevator.floor+1
      %{dir: :down} -> elevator.floor-1
    end
    new_elevator = %{elevator| floor: new_floor}
    IO.inspect {new_floor, elevator.dir}
    simulator(new_elevator, order, n_floors, cost+1)
  end


end
