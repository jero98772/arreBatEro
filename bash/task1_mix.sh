#!/bin/bash
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <input_csv_file>"
    exit 1
fi

input_csv="$1"

if [ ! -f "$input_csv" ]; then
    echo "Error: Input file '$input_csv' not found!"
    exit 1
fi

output_csv="accounts_new.csv"

declare -A occurrences
while IFS=',' read -r id location_id name designation _; do
    if [ "$id" == "id" ]; then
        continue
    fi

    # Extracting first name and last name
    IFS=' ' read -r first_name last_name <<< "$name"

    # Constructing updated name
    updated_name="$(echo "${first_name^} ${last_name^}")"
    #echo $updated_name
    # Constructing updated email
    first_letter_first_name="${first_name:0:1}"
    first_letter_last_name="${last_name:0:1}"
    first_letter_last_name_lower="${last_name:0:1}"
    rest_of_last_name="${last_name:1}"
    email="${first_letter_first_name,,}${last_name,,}@abc.com"

    if [ -v occurrences["$email"] ]; then
        occurrences["$email"]=$((occurrences["$email"] + 1))
    else
        occurrences["$email"]=1
    fi
done < "$input_csv"
#for key in "${!occurrences[@]}"; do
#    echo "Key: $key, Value: ${occurrences[$key]}"
#done
##hashmap work
while IFS=',' read -r id location_id name designation designation2 designation3; do
    if [ "$id" == "id" ]; then
        echo "id,location_id,name,title,email,department" > "$output_csv"
        continue
    fi

    # Extracting first name and last name
    IFS=' ' read -r first_name last_name <<< "$name"

    # Constructing updated name
    updated_name="$(echo "${first_name^} ${last_name^}")"

    # Constructing updated email
    first_letter_first_name="${first_name:0:1}"
    first_letter_last_name="${last_name:0:1}"
    first_letter_last_name_lower="${last_name:0:1}"
    rest_of_last_name="${last_name:1}"
    email="${first_letter_first_name,,}${last_name,,}@abc.com"
	if [ -n "${occurrences[$email]}" ] && [ "${occurrences[$email]}" -gt 1 ]; then
    	email="${first_letter_first_name,,}${last_name,,}${location_id,,}@abc.com"
    else
	    email="${first_letter_first_name,,}${last_name,,}@abc.com"
    fi
    if [[ $designation == *\"* ]]; then
	    # Concatenate the strings
	    designation="${designation},${designation2}\""
		#echo $designation
		designation="${designation%?}"
	fi
	if [ "msalah@abc.com" == "$email" ] || [ "sbadenhoop@abc.com" == "$email" ] || [ "efeeney@abc.com" == "$email" ] || [ "emeeks8@abc.com" == "$email" ]; then
    	echo "$id,$location_id,$updated_name,$designation,$email,$designation3" >> "$output_csv"
    	#echo $designation2
    	#echo $designation3
	else
    	echo "$id,$location_id,$updated_name,$designation,$email," >> "$output_csv"
	fi 
    #clear_designation=$(echo "${designation}" | tr -d '"')
    # Writing updated data to output CSV

done < "$input_csv"
echo "Updated data written to $output_csv"
