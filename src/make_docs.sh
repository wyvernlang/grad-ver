echo "creating .odoc for"
for f in *.cmi
do
  echo "  > $f"
  odoc compile --package=grad-ver $f
done

echo "creating .html for"
for f in *.odoc
do
  echo "  > $f"
  odoc html -o ../docs $f
done
