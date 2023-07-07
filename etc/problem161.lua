
-- Knuth's Algorithm X with dancing links.
--
-- Some subtleties are incorrect here, trying again in Python in problem161_1.py

local PointMeta = {
  __tostring=function (p) return p.x .. ',' .. p.y end,
  __add=function (p1, p2) return Point(p1.x + p2.x, p1.y + p2.y) end,
}

function Point(x, y)
  return setmetatable({x=x, y=y}, PointMeta)
end

local triominoes = {
  {Point(0, 0), Point(0, 1), Point(1, 0)}, -- flipped L
  {Point(0, 0), Point(1, 0), Point(1, 1)}, -- L rotated 180
  {Point(0, 0), Point(0, 1), Point(1, 1)}, -- L
  {Point(1, 0), Point(0, 1), Point(1, 1)}, -- mirrored L
  {Point(0, 0), Point(0, 1), Point(0, 2)}, -- I block
  {Point(0, 0), Point(1, 0), Point(2, 0)}, -- I block rotated 90
}

function construct_row(coordinates, source, triomino)
  local row = {}
  -- Initial values of zero.
  for _, _ in pairs(coordinates) do
    table.insert(row, 0)
  end
  -- Now identify and fill in all positions. If we ever encounter a
  -- position where there is no index for its coordinate, it's out of
  -- bounds, which means this is an invalid placement, so return nil.
  for _, relative_position in ipairs(triomino) do
    local coordinate_index = coordinates[tostring(source + relative_position)]
    if not coordinate_index then
      return nil
    end
    row[coordinate_index] = 1
  end
  return row
end

-- Returns a list of row lists.
function build_matrix(width, height)
  -- Fill out the coordinates grid.
  local coordinates = {}
  local coordinate_count = 0
  for x = 1, width do
    for y = 1, height do
      coordinate_count = coordinate_count + 1
      local point = Point(x, y)
      coordinates[tostring(point)] = coordinate_count
    end
  end

  -- Now build the matrix.
  local result = {}
  for x = 1, width do
    for y = 1, height do
      for _, triomino in ipairs(triominoes) do
        local row = construct_row(coordinates, Point(x, y), triomino)
        if row then
          table.insert(result, row)
        end
      end
    end
  end
  return result
end

function header()
  return {
    header=true,
    left=nil,
    right=nil,
    up=nil,
    down=nil,
  }
end

function node()
  return {
    header=false,
    left=nil,
    right=nil,
    up=nil,
    down=nil,
  }
end

function remove_node(node)
  node.left.right = node.right
  node.right.left = node.left
  node.up.down = node.down
  node.down.up = node.up
end

function reinsert_node(node)
  node.left.right = node
  node.right.left = node
  node.up.down = node
  node.down.up = node
end

function remove_column(start_node)
  local node = start_node
  repeat
    node.left.right = node.right
    node.right.left = node.left
    node = node.down
  until node == start_node
end

function reinsert_column(start_node)
  local node = start_node
  repeat
    node.left.right = node
    node.right.left = node
    node = node.down
  until node == start_node
end

function remove_row(start_node)
  local node = start_node
  repeat
    node.up.down = node.down
    node.down.up = node.up
    node = node.right
  until node == start_node
end

function reinsert_row(start_node)
  local node = start_node
  repeat
    node.up.down = node
    node.down.up = node
    node = node.right
  until node == start_node
end

function build_dancing_links(matrix)
  local width = #matrix[1]
  local height = #matrix
  -- Build header row
  local header_row = {}
  for i = 1, width do
    table.insert(header_row, header())
  end
  for i = 1, width - 1 do
    header_row[i].right = header_row[i + 1]
    header_row[i + 1].left = header_row[i]
  end
  header_row[#header_row].right = header_row[1]
  header_row[1].left = header_row[#header_row]
  -- Now, for each row, add columns
  for x, header_node in ipairs(header_row) do
    local last_node = header_node
    for y = 1, height do
      local new_node = node()
      last_node.down = new_node
      header_node.up = new_node
      new_node.up = last_node
      new_node.down = header_node
      last_node = new_node
    end
  end
  -- Link up the columns
  local current_row = header_row
  for y = 1, height + 1 do
    local new_row = {}
    for x = 1, width do
      table.insert(new_row, current_row[x].down)
      if x == width then
        current_row[width].right = current_row[1]
        current_row[1].left = current_row[width]
      else
        current_row[x].right = current_row[x + 1]
        current_row[x + 1].left = current_row[x]
      end
    end
    current_row = new_row
  end
  -- Now remove the zeroes
  local current_row = header_row
  for y = 1, height do
    local new_row = {}
    for x = 1, width do
      local curr = current_row[x].down
      table.insert(new_row, curr)
      if matrix[y][x] == 0 then
        remove_node(curr)
      end
    end
    current_row = new_row
  end
  -- Insert a control node which will never be removed, so we have
  -- some ground truth for who is in and is out of our table.
  local final_header = header()
  final_header.up = final_header
  final_header.down = final_header
  final_header.right = header_row[1]
  final_header.left = header_row[#header_row]
  header_row[1].left = final_header
  header_row[#header_row].right = final_header
  return final_header
end

function get_ones_in_column(header)
  local count = -1
  local node = header
  repeat
    count = count + 1
    node = node.down
  until header == node
  return count
end

function include_row(node)
  local j = node
  while j ~= j.right do
    local i = j
    while i ~= i.down do
      if not i.header then
        remove_row(i)
      end
      i = i.down
    end
    if not i.header then
      remove_row(i)
    end
    remove_column(j)
    j = j.right
  end
  remove_column(j)
end

function uninclude_row(node)
  local j = node.right
  while j ~= node do
    reinsert_column(j)
    local i = j.down
    while i ~= j do
      if not i.header then
        reinsert_row(i)
      end
      i = i.down
    end
    j = j.right
  end
end

function count_columns(links)
  local node = links
  local count = -1
  repeat
    count = count + 1
    node = node.right
  until node == links
  print(count)
end

function run_algorithm_x(links)
  local total = 0
  local function recurse()
    count_columns(links)
    if links.up == links and links.down == links and links.left == links and links.right == links then
      -- We found a solution! Huzzah! Count it and return.
      total = total + 1
      return
    end
    -- Find the column with the minimum number of ones
    local node = links.right
    local best_node = nil
    local best_ones = math.huge
    while node ~= links do
      local ones = get_ones_in_column(node)
      if ones < best_ones then
        best_node = node
        best_ones = ones
      end
      node = node.right
    end
    -- Go through each row and do the algorithm
    node = best_node.down
    while node ~= best_node do
      include_row(node)
      recurse()
      uninclude_row(node)
      node = node.down
    end
  end
  recurse()
  return total
end

local matrix = build_matrix(2, 3)
local links = build_dancing_links(matrix)

print(run_algorithm_x(links))
