# Generated by Yosys 0.31+16 (git sha1 c7670b36d, clang 14.0.3 -fPIC -Os)
autoidx 1
attribute \top 1
attribute \src "wrapper.v:1.1-3.10"
module \uut
  attribute \src "wrapper.v:1.28-1.29"
  wire width 9 input 1 \A
  attribute \src "wrapper.v:1.44-1.45"
  wire output 2 \Y
  attribute \hdlname "uut A"
  attribute \src "wrapper.v:2.5-2.56|uut.v:1.24-1.25"
  wire \uut.A
  attribute \hdlname "uut B"
  attribute \src "wrapper.v:2.5-2.56|uut.v:1.39-1.40"
  wire width 4 \uut.B
  attribute \hdlname "uut S"
  attribute \src "wrapper.v:2.5-2.56|uut.v:1.54-1.55"
  wire width 4 \uut.S
  attribute \hdlname "uut Y"
  attribute \src "wrapper.v:2.5-2.56|uut.v:1.70-1.71"
  wire \uut.Y
  attribute \hdlname "uut cell"
  attribute \module_not_derived 1
  attribute \src "wrapper.v:2.5-2.56|uut.v:2.34-2.67"
  cell $pmux \uut.cell
    parameter signed \S_WIDTH 4
    parameter signed \WIDTH 1
    connect \A \A [0]
    connect \B \A [4:1]
    connect \S \A [8:5]
    connect \Y \uut.Y
  end
  connect \uut.A \A [0]
  connect \uut.B \A [4:1]
  connect \uut.S \A [8:5]
  connect \Y \uut.Y
end
