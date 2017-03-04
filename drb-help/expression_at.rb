line_number = @drb_args.last
file_name = @drb_args.first
file_content = open(file_name){ |f| f.read }
expression_at(file_content, line_number.to_i)
