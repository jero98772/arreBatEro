sed -i 's/caremonda/tu cara de butiffarra/g' filename.txt
sed -i '/^$/d' filename.txt
sed -i 's/$/ new_text/' filename.txt
sed = filename.txt | sed 'N;s/\n/\t/'

sed -i 's/\bbutiffarra\b/caremonda2/g' filename.txt

sed -i '/new_text/s/^/gg/' filename.txt

sed -i 's/\(.*\) \(.*\)/\2 \1/g' filename.txt

sed -i 's/\(.*\) \(.*\)/\2 \1/g' filename2.txt

sed -i 's/Training/UltraTraining/gI' filename.txt
