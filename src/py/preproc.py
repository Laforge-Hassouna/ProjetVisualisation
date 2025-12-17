import pandas as pd
import os
import sys

def shuffle_and_extract(input_csv_path, output_csv_path, num_lines):
    """
    Shuffles a CSV dataset, extracts the first `num_lines` lines, and saves them to a new CSV file.

    Args:
        input_csv_path (str): Path to the input CSV file.
        output_csv_path (str): Path to save the output CSV file.
        num_lines (int): Number of lines to extract after shuffling. Default is 10000.
    """
    # Load the dataset
    try:
        df = pd.read_csv(input_csv_path)
    except Exception as e:
        print(f"Error loading {input_csv_path}: {e}")
        return

    # Shuffle the dataset
    df = df.sample(frac=1, random_state=42).reset_index(drop=True)

    # Extract the first `num_lines` lines
    df_extracted = df.head(num_lines)

    # Save the extracted lines to a new CSV file
    try:
        df_extracted.to_csv(output_csv_path, index=False)
        print(f"Successfully saved the first {num_lines} shuffled lines to {output_csv_path}")
    except Exception as e:
        print(f"Error saving to {output_csv_path}: {e}")

if __name__ == "__main__":
    # Example usage
    input_csv_path = "./airline-delay-cause/Airline_Delay_Cause.csv"
    output_csv_path = sys.argv[1]
    number_of_line = int(sys.argv[2])

    shuffle_and_extract(input_csv_path, output_csv_path, number_of_line)
