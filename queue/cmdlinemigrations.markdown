A while ago I had a neat idea for sql DDL management. It's not necessarily
something I would use in production, but a neat idea nonetheless. Like in my
previous posts, I prefer having small changes in SQL-files which have
dependency information.

Let's say for example that you have a change that changes email field size. It
would depend on whatever change last changed the field.

> temp=/tmp/out.txt

> trap "rm $temp" EXIT

> for file in sql/*.sql; do
>     dline=$(gawk -F ':' '/^depends:/ {print $2}' $file)
>     currentFile=$(basename $file)
>     current=${currentFile%.sql}
>     echo "$current $dline" >> $temp
> done

> tsort $temp | tac | while read line; do
>     file="sql/${line}.sql"
>     grep -v "^depends:" $file
> done

