
Pos = Struct.new(:x, :y, :z) do

  class <<self
    alias :[] :new
  end

  def each_adjacent(&block)
    if block
      block.call Pos[x - 1, y, z]
      block.call Pos[x + 1, y, z]
      block.call Pos[x, y - 1, z]
      block.call Pos[x, y + 1, z]
      block.call Pos[x, y, z - 1]
      block.call Pos[x, y, z + 1]
    else
      Enumerator.new { |y| self.each_adjacent { |p| y << p } }
    end
  end

end

class CuboidShape
  attr_reader :current_layer

  def initialize
    @contents = {}
    @current_layer = 1
  end

  def include?(pos)
    @contents.include? pos
  end

  def [](pos)
    @contents[pos]
  end

  def []=(pos, value)
    if @contents.include? pos
      @contents[pos] = [@contents[pos], value].min
    else
      @contents[pos] = value
    end
  end

  def each_key(&block)
    @contents.keys.each(&block)
  end

  def each_value(&block)
    @contents.values.each(&block)
  end

  def add_layer
    @current_layer += 1
    self.each_key do |pos|
      pos.each_adjacent do |pos1|
        self[pos1] = @current_layer
      end
    end
  end

  def total_in_layer(layer)
    self.each_value.count(layer)
  end

  def self.cuboid(x, y, z)
    self.new.tap do |c|
      (1..x).to_a.product((1..y).to_a, (1..z).to_a) do |x, y, z|
        c[Pos.new(x, y, z)] = 1
      end
    end
  end

end

UP_TO = 154

c = {}
c.default = 0
(1..UP_TO).each do |x|
  (1..x).each do |y|
    (1..y).each do |z|
      cuboid = CuboidShape.cuboid x, y, z
      while cuboid.total_in_layer(cuboid.current_layer) < UP_TO
        cuboid.add_layer
        c[cuboid.total_in_layer(cuboid.current_layer)] += 1
      end
    end
  end
end
puts c
