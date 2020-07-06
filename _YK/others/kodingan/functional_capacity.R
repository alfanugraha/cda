system_func_capacity<-data.frame(
  id=c(1:18),
  func_capacity=c(
    "Regulasi: 1.1 Terkait Kaji Ulang RAD GRK /PPRKD",
    "Regulasi: 1.2 Tingkat operasionalisasi regulasi Kaji Ulang RAD GRK /PPRKD",
    "Integrasi dalam Perencanaan Pembangunan Daerah: 2.1 Visi Misi",
    "Integrasi dalam Perencanaan Pembangunan Daerah: 2.2 Isu Strategis Pembangunan",
    "Integrasi dalam Perencanaan Pembangunan Daerah: 2.3 Prioritas Pembangunan Daerah",
    "Integrasi dalam Perencanaan Pembangunan Daerah: 2.4 Indikator Pembangunan",
    "Integrasi dalam Perencanaan Pembangunan Daerah: 2.5 Program dan Kegiatan Pembangunan Daerah",
    "Proses: 3.1 Pelaksanaan Kaji Ulang RAD GRK /PPRKD",
    "Proses: 3.2 Kerjasama lembaga Pelaksanaan Kaji Ulang RAD GRK /PPRKD",
    "Proses: 3.3 Tahapan",
    "Proses: 3.4 Pelibatan Masyarakat",
    "Proses: 3.5 Proses pengarusutamaan",
    "Data dan Informasi: 7.1 Ketersediaan dan Aksesibilitas Data",
    "Data dan Informasi: 7.2 Kualitas Data",
    "Data dan Informasi: 7.3 Pengelolaan Data",
    "Pemantauan Evaluasi dan Pelaporan: 9.1 Muatan/Substansi",
    "Pemantauan Evaluasi dan Pelaporan: 9.2 Pelaksanaan",
    "Pemantauan Evaluasi dan Pelaporan: 9.3 Pelaksana"
  )
)

organisation_func_capacity<-data.frame(
  id=c(1:15),
  func_capacity=c(
    "Perangkat Pelaksana: 4.1 Penentuan Visi, Misi dan Tujuan",
    "Perangkat Pelaksana: 4.2 Struktur organisasi",
    "Perangkat Pelaksana: 4.3 Proses pengambilan keputusan",
    "Perangkat Pelaksana: 4.4 Prosedur/Proses Kerja dan Pengelolaan lembaga",
    "Perangkat Pelaksana: 4.5 Budgeting",
    "Perangkat Pelaksana: 4.6 Hubungan Internal",
    "Perangkat Pelaksana: 4.7 Networking/Hubungan Eksternal",
    "SDM: 5.1 Teknis",
    "SDM: 5.2 Mainstreaming",
    "SDM: 5.3 Penulisan dan Pelaporan",
    "SDM: 5.4 Dinamika Organisasi",
    "SDM: 5.5 Peningkatan Kapasitas",
    "Teknologi: 8.1 Perangkat Lunak",
    "Teknologi: 8.2 Perangkat Keras",
    "Teknologi: 8.3 Perangkat Jaringan"
  )
)

individu_func_capacity<-data.frame(
  id=c(1:4),
  func_capacity=c(
    "SDM: 6.1 Kesesuaian Peran dalam Implementasi RAD GRK/PPRKD dengan Tugas dan Fungsi", 
    "SDM: 6.2 Pengetahuan",
    "SDM: 6.3 Keterampilan", 
    "SDM: 6.4 Pengembangan dan Motivasi" 
  )
)

cda_system <- read.table("init/system.csv", header = TRUE, sep = ",")
cda_organisation <- read.table("init/organisation.csv", header = TRUE, sep = ",")
cda_individu <- read.table("init/individu.csv", header = TRUE, sep = ",")

