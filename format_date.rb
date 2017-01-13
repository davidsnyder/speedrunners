puts %w(player format_time date).join("\t")
$stdin.drop(1).each do |row|
  player,time,date = row.strip.split("\t")
  m,s,ms = time.split(":")
  h = "00"
  fixed_time = [h,m,s].join(":") + "." + ms
  puts [player,fixed_time,date].join("\t")
end
