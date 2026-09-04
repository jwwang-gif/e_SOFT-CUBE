# e_SOFT CUBE

**e_SOFT CUBE** is a Fortran program that reconstructs high-resolution urban wind and air-temperature fields from a pre-computed CFD database and LDAPS meteorological profiles. It is the extended-domain implementation of SOFT CUBE: sixteen local CFD domains (`D01`–`D16`) are reconstructed independently and joined into one composite field.

The program is intended for an HPC environment containing the complete SOFT CUBE database and its preprocessed LDAPS and surface-temperature inputs. The database is not included in this repository.

## What this program does

For each target time and vertical level, `e_SOFTCUBE.f90`:

1. Reads the urban geometry mask and seven reference inflow-wind profiles.
2. Reads LDAPS `U`, `V`, and temperature profiles for each of the 16 subdomains.
3. Converts the LDAPS wind components to wind speed and direction and converts temperature from °C to K.
4. Reads six surface-to-air temperature ratios for each subdomain: roof, road, green area, soil, wall, and water.
5. Selects the surrounding CFD database cases for wind speed, wind direction, and surface-temperature class.
6. Interpolates the selected CFD fields and superposes the six land-cover contributions within each subdomain.
7. Assembles `D01`–`D16` into a 4 × 4 composite grid, using 10-grid-wide linear or bilinear transition zones between neighboring domains.
8. Writes the resulting `u`, `v`, `w`, and air-temperature values.

## Implemented grid

The dimensions below are taken directly from the current source code.

| Component | Source-code dimensions | Role |
| --- | ---: | --- |
| Local CFD domain | `158 × 158` allocated | Includes the `156 × 156` base grid plus boundary indexing |
| Composite domain | `602 × 602` | Four-by-four assembly of the 16 local domains |
| Vertical dimension | `62` levels | `kmax2`, derived from `kmax = 60` |
| Tile core used in assembly | `148 × 148` | Non-overlap portion retained from each local domain |
| Tile transition width | `10` grid points | Linear edge blending and bilinear corner blending |

The composite layout in the program is:

```text
D01  D02  D03  D04
D05  D06  D07  D08
D09  D10  D11  D12
D13  D14  D15  D16
```

## Reconstruction method

### Meteorological interpolation

The program derives wind speed and direction from each LDAPS `U`/`V` profile. At every vertical level it locates neighboring reference wind-speed and direction cases in the CFD database and combines those cases using interpolation weights. Values outside the available wind-speed range are scaled from the lowest or highest reference case.

### Surface thermal response

For every subdomain, RSA input supplies the surface-to-air temperature ratios for:

- roof
- road
- green area
- soil
- wall
- water

The corresponding CFD temperature-response fields are selected and interpolated by surface class. The six land-cover contributions are then combined to reconstruct the local air-temperature field.

### Domain composition

Each tile contributes a `148 × 148` core. Adjacent cores are separated by 10-point transition zones. One-dimensional linear interpolation is used along shared tile edges, while bilinear interpolation combines four neighboring tiles at transition-zone intersections. This produces the final `602 × 602` horizontal field.

## Required data

The source expects the following directory groups under the repository-local `data/` directory:

| Input group | Purpose |
| --- | --- |
| `Input/Beaufort/Inflow_01.txt`–`Inflow_07.txt` | Reference inflow wind-speed profiles |
| `Input/RSA_2/01`–`16` | Time-dependent surface-to-air ratios for the 16 domains |
| `Input/ML_LDAPS_Profile/01`–`16` | Time-dependent LDAPS `U`, `V`, and temperature profiles |
| `6km_DB/Layer/D01`–`D16` | CFD fields organized by land-cover class, wind-speed case, level, and direction |

CFD database records contain eight real values per horizontal cell. The current code reads the wind components and thermal quantities used in reconstruction from these records. Output records contain four values per cell.

## Configuration before running

Prepare the runtime layout before starting a calculation:

1. Place or link the complete input hierarchy at `data/`.
2. Create an `output/` directory in the working directory.
3. Confirm that the input filenames and database hierarchy match the templates described above.
4. Ensure sufficient memory and storage are available. The program declares many large two-dimensional arrays and repeatedly reads CFD fields for all tiles and surface classes.

## Building

GNU Fortran or another compiler supporting free-form Fortran can be used. The source contains lines longer than the default free-form limit, so GNU Fortran requires `-ffree-line-length-none`.

```bash
gfortran -O2 -ffree-line-length-none -o e_SOFTCUBE e_SOFTCUBE.f90
```

For debugging a newly configured installation, bounds and runtime checks are recommended:

```bash
gfortran -O0 -g -ffree-line-length-none -fcheck=all -fbacktrace \
  -o e_SOFTCUBE_debug e_SOFTCUBE.f90
```

Depending on the compiler, platform, and memory model, additional options may be necessary because of the program's large static data footprint.

## Running

Run the program from the repository root and supply one target date-time in `YYYYMMDDHH` format:

```bash
mkdir -p output
./e_SOFTCUBE 2021070100
```

The program validates the argument before opening input files. Progress is printed as each vertical level is processed.

## Output

The output filename is constructed as:

```text
output/e_SOFTCUBE_YYYYMMDDHH.dat
```

Each record contains:

```text
u  v  w  air_temperature
```

using the Fortran format `4(1x,f12.6)`. For each vertical level, values are written over the full `602 × 602` horizontal grid with `i` varying inside `j`. All processed levels are appended to the same time-stamped composite file during the run.

## Repository contents

| File | Description |
| --- | --- |
| [`e_SOFTCUBE.f90`](./e_SOFTCUBE.f90) | Main reconstruction, interpolation, tile-composition, and output program |
| [`README.md`](./README.md) | Program description and execution requirements |

## Data availability

The complete CFD database is several terabytes in size and is hosted on an internal HPC server subject to institutional security policies.

- The complete database cannot currently be hosted publicly.
- e_SOFT CUBE requires the complete database; partial database operation is not supported.
- This repository provides the source code and program documentation.
- Database access may be granted through a formal data-sharing agreement, subject to institutional approval.

## Software information

| Item | Details |
| --- | --- |
| Software | e_SOFT CUBE |
| Full name | Extended Synthesis of Flows and Temperatures based on a CFD model and urban building environments |
| Developers | Jang-Woon Wang and Jae-Jin Kim |
| Contact | [jwwang@pukyong.ac.kr](mailto:jwwang@pukyong.ac.kr) |
| First available | August 2025 |
| Language | Fortran |
| Repository | [github.com/jwwang-gif/SOFT-CUBE](https://github.com/jwwang-gif/SOFT-CUBE) |
