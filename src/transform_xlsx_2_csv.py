import os

all_files_in_data = [file for _, _, file in os.walk("/workdir/data/serie_a")][0]
all_files_xlsx = [file.split(".")[0].split("Team Stats ")[1] for file in all_files_in_data if file.split(".")[1] == "xlsx"]
files_to_change = [file.replace(" ","_") for file in all_files_xlsx]


def return_transformation_command(xlsx_files: str, file_to_change):
    return f"in2csv '/workdir/data/serie_a/{xlsx_files}' > /workdir/data/{file_to_change}.csv"


for old_file, new_file in zip(all_files_in_data, files_to_change):
    os.system(return_transformation_command(old_file, new_file))