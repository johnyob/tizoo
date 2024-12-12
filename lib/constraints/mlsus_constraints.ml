module Constraint = struct
  include Constraint

  let solve = Solver.solve
end
