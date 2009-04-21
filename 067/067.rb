
table_txt = File.read('triangle.txt')

table = table_txt.split("\n").map{|str|str.split.map{|num|num.to_i}}
#p table

def max(a,b)
  a>b ? a : b
end

res_table = table.dup
(table.size-2).downto(0) do |i|
  row = res_table[i]
  next_row = res_table[i+1]
  for j in (0..row.size-1)
    row[j] += max(next_row[j], next_row[j+1]) 
  end
end

#p res_table

puts "The result is: #{res_table[0][0]}"


