defmodule HeisProsjekt do

  def hello(port \\9000) do
    # Remember to use epmd -daemon
    IO.puts("I am recompiled")
    IO.puts("Hello world")
    ConnectionEstablisher.start_link(:filler_arg)
    Driver.start_link(%{ip: {127,0,0,1}, port: port, n_floors: 4, floor_subscriber_funs: [&FiniteStateMachine.arrive_floor/1]})
    FiniteStateMachine.start_link(%{n_floors: 4})

    {:ok, queue_pid} = LocalOrderHandler.start_link(:filler)
    LocalOrderHandler.put_order node(), 2 , :hall_up
    LocalOrderHandler.put_order node(), 2 , :cab
    # LocalOrderHandler.put_order "Other_node", 2 , :cab
    # LocalOrderHandler.floor_served node(), 1, :hall_down
    # LocalOrderHandler.floor_served node(), 1, :cab
    # QueueMaster.claim_an_order node(), 1, :up
    # QueueMaster.claim_an_order "Other_node", 1, :up
    # QueueMaster.claim_an_order "Or_node", 1, :up
    {:ok, queue_pid}
  end
end
