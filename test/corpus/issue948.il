# https://github.com/YosysHQ/yosys/issues/948
module \top
  wire width 1 input 0 $signal
  wire width 1 output 1 \a
  wire width 1 $next\a
  wire width 1 $1
  cell $reduce_bool $2
    parameter \A_SIGNED 0
    parameter \A_WIDTH 0
    parameter \Y_WIDTH 1
    connect \A {}
    connect \Y $1
  end
  process $group_0
    assign $next\a 1'0
    assign $next\a $1
    sync init
    sync always
      update \a $next\a
  end
end
