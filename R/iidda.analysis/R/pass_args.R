pass_args = function(data, fn) {
  arg_nms = fn |> formals() |> names()
  all_args = parent.frame() |> as.list()
  args = all_args[intersect(names(all_args), arg_nms)]
  args$data = data
  do.call(fn, args)
}
