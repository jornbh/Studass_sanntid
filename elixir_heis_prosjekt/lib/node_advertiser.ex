defmodule ConnectionEstablisher do

  def start_link() do
    register_node_name()
    listen_pid = NodeListener.start_link()
    broadcaster_pid = NodeAdvertiser.start_link()
    IO.inspect listen_pid
    IO.inspect broadcaster_pid
  end

  # Info needed to register a nodename
  def get_IP_str do
    {:ok, ifs} = :inet.getif()
    ips = Enum.map(ifs, fn {ip, _broadaddr, _mask} -> ip end)
    valid_ips = ips -- [{127,0,0,1}] # Add more illegal ips if there are any
    ip_tuple = hd(valid_ips)
    to_string(:inet_parse.ntoa(ip_tuple))
  end

  def register_node_name(node_number \\ 0) do
    ip_str = get_IP_str()
    index_str = Integer.to_string(node_number)
    full_name_str = "foobar" <> index_str <>"@"<> ip_str
    full_name_atom = String.to_atom(full_name_str)
    case Node.start(full_name_atom) do
      {:ok, result} ->
        IO.write "Node started with result: "
        IO.inspect result;
      {:error, _result}-> register_node_name( node_number + 1)
    end
  end
end

defmodule NodeListener do
  def start_link() do
    spawn_link(__MODULE__, :main, [])
  end
  def rcv_port do 5675 end
  def main do
    {:ok, recv_socket} = :gen_udp.open(rcv_port(), [:list, {:reuseaddr,true}, {:active,false}])
    IO.write "Spawned Node-listener listens on #{rcv_port()} with a socket "
    IO.inspect recv_socket
    recv_loop(recv_socket)
  end

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
    def send_port do  5676 end
    def send_IP do  {255,255,255,255} end
    def broadcast_period do 2000 end
    def start_link() do
      # IO.puts "Hello from Node-advertiser start on #{@send_port}"
      spawn_link(__MODULE__, :main, [])
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
