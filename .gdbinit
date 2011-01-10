set print pretty on
set height -1

define pglobal
  print global_$arg0[cur_stack_id]
end
