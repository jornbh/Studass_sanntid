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
  def start_link(args) do
    GenServer.start_link( __MODULE__, args,name: {:global, __MODULE__} )
  end

  def get_order(node) do
    GenServer.call(__MODULE__, {:get_order, node})
  end

  # Internal functions
  ##################################################################

  @impl true
  def init(_args) do
    {:ok, :empty_state}
  end
  @impl true
  def handle_call(message, _from, state) do
    IO.puts "Called"
    {message, state}
  end
end


defmodule LocalQueue do
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

  def claim_and_get_a_destination(node, floor, dir) do
    GenServer.call(__MODULE__, {:claim_and_get_a_destination, node, floor, dir},90000000)
  end

  def put_order(node, floor, dir) do
    GenServer.cast(__MODULE__, {:put_order, node, floor, dir})
  end
  def get_world() do
    GenServer.call(__MODULE__, :get_world)
  end

  # Internal functions
  ##################################################################

  @impl true
  def init(_args) do
    world = %{reserved_orders: %{node() => :none}, free_orders: [] }
    {:ok, world}
  end
  @impl true
  def handle_call(:get_world, _from, world)do
    {:reply, world, world}
  end
  def handle_call({:claim_and_get_a_destination, node, floor, dir}, _from, world) do

    free_orders = world.free_orders
    elevator_info = {node, floor, dir}
    cost_fun = fn(order) -> cost_function( elevator_info, order, 4 ) end
    cheapest = Enum.min_by( free_orders, cost_fun, fn()-> :none end )
    reserved_orders = world.reserved_orders
    new_free_orders =case reserved_orders[node] do
      :nil -> free_orders -- [cheapest]
      :none -> free_orders -- [cheapest]
      _-> (free_orders -- [cheapest]) ++ [reserved_orders[node]]
    end

    new_reserved_orders =Map.put(world.reserved_orders,node , cheapest)
    new_world = %{world |
                reserved_orders: new_reserved_orders,
                free_orders: new_free_orders}
    {:reply, cheapest, new_world}
  end
  @impl true
  def handle_cast({:put_order, node, floor, dir},  world) do
    IO.puts "Called"
    new_order = {node, floor, dir}
    new_free_orders = [new_order] ++ (world.free_orders -- [new_order])
    new_world = %{world| free_orders: new_free_orders}
    {:noreply, new_world}
  end


  def cost_function({e_node,_,_}, {o_node,_,:cab}) when e_node != o_node do
    90000000000000 # Infinite cost
  end
  def cost_function( elevator_info, order_info, n_floors) do
    IO.inspect elevator_info
    {_, e_floor, e_dir} = elevator_info
    {_, o_floor, o_dir} = order_info
  must_turn = cond do
    e_dir == :still                      -> false
    e_dir != o_dir and o_dir != :cab     -> true
    e_dir == :up   and e_floor > o_floor -> true
    e_dir == :down and e_floor < o_floor -> true
    true                                 -> false
  end
  dist = abs( e_floor - o_floor )
  case must_turn do
    true -> n_floors + dist
    _    -> dist
  end
  end

end
