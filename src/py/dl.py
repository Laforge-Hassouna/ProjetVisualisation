import kagglehub

# Download latest version
path = kagglehub.dataset_download("abdelazizel7or/airline-delay-cause")

print("Path to dataset files:", path)