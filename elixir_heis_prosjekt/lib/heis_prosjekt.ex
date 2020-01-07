defmodule HeisProsjekt do

  def hello do
    # Remember to use epmd -daemon
    IO.puts("I am recompiled")
    IO.puts("Hello world")
    # ConnectionEstablisher.start_link(:filler_arg)
    # Driver.start_link(%{ip: {127,0,0,1}, port: 9000, n_floors: 4, floor_subscriber_funs: [&FiniteStateMachine.arrive_floor/1]})
    # {:ok, fsm_pid} = FiniteStateMachine.start_link(%{n_floors: 4})

    {:ok, queue_pid} = LocalQueue.start_link(:filler)
    # LocalQueue.put_order(node(), 1, :cab)
    # LocalQueue.put_order(node(), 3, :up)
    LocalQueue.claim_and_get_a_destination(node(), 2, :down)

    {:ok, queue_pid}
  end
end
