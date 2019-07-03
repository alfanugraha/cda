system_func_capacity<-data.frame(
  id=c(1:18),
  func_capacity=c(
    "1.1. Terkait Kaji Ulang RAD GRK /PPRKD",
    "1.2. Tingkat operasionalisasi regulasi Kaji Ulang RAD GRK /PPRKD",
    "2.1. Visi Misi",
    "2.2. Isu Strategis Pembangunan",
    "2.3. Prioritas Pembangunan Daerah",
    "2.4. Indikator Pembangunan",
    "2.5. Program dan Kegiatan Pembangunan Daerah",
    "3.1. Pelaksanaan Kaji Ulang RAD GRK /PPRKD",
    "3.2. Kerjasama lembaga Pelaksanaan Kaji Ulang RAD GRK /PPRKD",
    "3.3. Tahapan",
    "3.4. Pelibatan Masyarakat",
    "3.5. Proses pengarusutamaan",
    "7.1. Ketersediaan dan Aksesibilitas Data",
    "7.2. Kualitas Data",
    "7.3. Pengelolaan Data",
    "9.1. Muatan/Substansi",
    "9.2. Pelaksanaan",
    "9.3. Pelaksana"
  )
)

organisation_func_capacity<-data.frame(
  id=c(1:15),
  func_capacity=c(
    "4.1. Penentuan Visi, Misi dan Tujuan",
    "4.2. Struktur organisasi",
    "4.3. Proses pengambilan keputusan",
    "4.4. Prosedur/Proses Kerja dan Pengelolaan lembaga",
    "4.5. Budgeting",
    "4.6. Hubungan Internal",
    "4.7. Networking/Hubungan Eksternal",
    "5.1. Teknis",
    "5.2. Mainstreaming",
    "5.3. Penulisan dan Pelaporan",
    "5.4. Dinamika Organisasi",
    "5.5. Peningkatan Kapasitas",
    "8.1. Perangkat Lunak",
    "8.2. Perangkat Keras",
    "8.3. Perangkat Jaringan"
  )
)

individu_func_capacity<-data.frame(
  id=c(1:4),
  func_capacity=c(
    "6.1. Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi", 
    "6.2. Pengetahuan",
    "6.3. Keterampilan", 
    "6.4. Pengembangan dan Motivasi" 
  )
)

cda_system <- read.table("init/system.csv", header = TRUE, sep = ",")
cda_organisation <- read.table("init/organisation.csv", header = TRUE, sep = ",")
cda_individu <- read.table("init/individu.csv", header = TRUE, sep = ",")

