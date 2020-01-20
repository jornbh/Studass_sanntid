defmodule ConnectionEstablisher do
  #! Remember to call epmd -daemon if the program crashes inexplicably
  use Supervisor
  @spec start_link(any) :: :ignore | {:error, any} | {:ok, pid}
  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end
  def init(_init_arg) do
    register_node_name()
    children = [NodeListener, NodeAdvertiser]
    Supervisor.init(children, strategy: :one_for_one)
  end

  # Private functions
  def get_IP_str do
    {:ok, ifs} = :inet.getif()
    ips = Enum.map(ifs, fn {ip, _broadaddr, _mask} -> ip end)
    valid_ips = ips -- [{127,0,0,1}] # Add more illegal ips if there are any
    ip_tuple = hd(valid_ips)
    to_string(:inet_parse.ntoa(ip_tuple))
  end

  def register_node_name(node_number \\ 0)
  def register_node_name(node_number) when node_number <= 200  do
    ip_str = get_IP_str()
    index_str = Integer.to_string(node_number)
    full_name_str = "myNode" <> index_str <>"@"<> ip_str
    full_name_atom = String.to_atom(full_name_str)
    case Node.start(full_name_atom) do
      {:ok, result} ->
        Node.set_cookie(:elevator_cookie)
        IO.write "Node started with result: "
        IO.inspect result;
      {:error, result}->
        IO.inspect result
        register_node_name( node_number + 1)
    end
  end
  def register_node_name(_node_number) do
    IO.puts "Tried to register a name 200 times, so I will just kill myself"
    exit("Tried to register a name more that 200 times and failed")
  end
end

defmodule NodeListener do
  def child_spec(_arg) do
    %{
    id: NodeListener,
    start: {NodeListener, :start_link, []}
  }
  end
  @spec start_link :: {:ok, pid}
  def start_link() do
    pid = spawn_link(__MODULE__, :main, [])
    {:ok, pid}
  end
  def rcv_port do 5675 end

  @spec main :: no_return
  def main do
    {:ok, recv_socket} = :gen_udp.open(rcv_port(), [:list, {:reuseaddr,true}, {:active,false}])
    IO.write "Spawned Node-listener listens on #{rcv_port()} with a socket "
    IO.inspect recv_socket
    recv_loop(recv_socket)
  end

  @spec recv_loop(port) :: no_return
  def recv_loop(  recv_socket) do
    {:ok, {_IP, _port, node_ID}} = :gen_udp.recv(recv_socket,0) #This blocks without another broadcaster
    # IO.write "Return Val"
    # IO.inspect node_ID
    add_node_to_cluster( node_ID )
    recv_loop( recv_socket )
  end

  def add_node_to_cluster( node_ID ) do
    known_nodes = [node() | Node.list()]
    node_strings = Enum.map( known_nodes, &Atom.to_string/1 )
    node_ID_str = List.to_string(node_ID)
    unless Enum.member?(node_strings, node_ID_str) do
      node_ID_atom = :erlang.list_to_atom(node_ID)
      Node.ping(node_ID_atom)
      IO.puts "Added node #{node_ID} to cluster"
      #TODO Resolve the world-views of the merging clusters
    end
  end
end

defmodule NodeAdvertiser do

  def child_spec(_arg) do
    %{
    id: NodeAdvertiser,
    start: {NodeAdvertiser, :start_link, []}
  }
  end
    def send_port do  5676 end
    def send_IP do  {255,255,255,255} end
    def broadcast_period do 2000 end
    def start_link() do
      # IO.puts "Hello from Node-advertiser start on #{@send_port}"
      pid = spawn_link(__MODULE__, :main, [])
      {:ok, pid}
    end
    def main do
      options = [:list, {:reuseaddr,true}, {:active,true}, {:broadcast, true}]
      {:ok, socket} = :gen_udp.open(send_port(), options)
      broadcast_loop(socket)
    end

    @spec broadcast_loop(port) :: no_return
    def broadcast_loop(socket) do
      node_name =:erlang.atom_to_list( node() )
      recv_port = NodeListener.rcv_port()
      :ok = :gen_udp.send( socket, send_IP() , recv_port, node_name )

      n_nodes = 1+ length( Node.list() )
      :timer.sleep(broadcast_period()*n_nodes)
      # IO.puts "Done sleeping"
      broadcast_loop(socket)
    end
end
