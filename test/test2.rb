require 'rubygems'
require 'sexpistol'

def map_sub(arr)
  return [] if arr.nil?
  return arr if !arr.is_a?(Array)
  return [] if arr.size == 0
  return {arr[0] => []} if arr.size == 1
  return {arr[0] => map_sub(arr[1])} if arr.size == 2

  res = {arr[0] => []}
  
  arr.each_with_index do |item,i|
    next if i == 0
    res[arr[0]] << map_sub(item)
  end

  res
end

def map(arr)
  return map_sub(arr[0]) if arr.size == 1
  return map_sub(arr)
end

s = Sexpistol.new
e = s.parse_string "(create (vds (ram 256) (eth \"1\" \"2\")))"

p e
p map(e)
