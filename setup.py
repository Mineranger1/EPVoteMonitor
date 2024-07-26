import os

def create_directory_structure(base_dir):
    folders = ["EP6_clean_data", "EP7_clean_data", "EP8_clean_data", "EP9_clean_data"]
    base_path = os.path.join(base_dir, "Cleaned_data")
    
    try:
        os.makedirs(base_path)
        print(f"Created base directory: {base_path}")
    except FileExistsError:
        print(f"Base directory already exists: {base_path}")

    for folder in folders:
        path = os.path.join(base_path, folder)
        try:
            os.makedirs(path)
            print(f"Created subdirectory: {path}")
        except FileExistsError:
            print(f"Subdirectory already exists: {path}")

def install_packages():
    import subprocess
    subprocess.check_call(["pip", "install", "-r", "requirements.txt"])

if __name__ == "__main__":
    create_directory_structure(os.getcwd())
    install_packages()
