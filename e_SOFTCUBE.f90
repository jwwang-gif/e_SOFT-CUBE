PROGRAM E_SOFTCUBE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! e_SOFT CUBE public release
! Test time is fixed to 2021-07-04 22 UTC.
! Input data are resolved relative to the data/ directory.
! Create the output/ directory before running the program.

      integer,parameter :: imax = 156, jmax = 156, kmax = 60 
      integer,parameter :: imax1 = imax+1,  jmax1 = jmax+1,  kmax1 = kmax+1 
      integer,parameter :: imax2 = imax1+1, jmax2 = jmax1+1, kmax2 = kmax1+1 

      integer,parameter :: eimax = 600, ejmax = 600
      integer,parameter :: eimax1 = eimax+1,  ejmax1 = ejmax+1
      integer,parameter :: eimax2 = eimax1+1, ejmax2 = ejmax1+1

      integer:: mm, dd, hh
      
      real,dimension(imax2, jmax2) :: uu1, vv1, ww1, ftemp1, uu2, vv2, ww2, ftemp2
      real,dimension(imax2, jmax2) :: uu3, vv3, ww3, ftemp3, uu4, vv4, ww4, ftemp4
      real,dimension(imax2, jmax2) :: uu5, vv5, ww5, ftemp5, uu6, vv6, ww6, ftemp6
      real,dimension(imax2, jmax2) :: uu7, vv7, ww7, ftemp7, uu8, vv8, ww8, ftemp8
      real,dimension(imax2, jmax2) :: uu9, vv9, ww9, ftemp9, uu10, vv10, ww10, ftemp10
      real,dimension(imax2, jmax2) :: uu11, vv11, ww11, ftemp11, uu12, vv12, ww12, ftemp12
      real,dimension(imax2, jmax2) :: uu13, vv13, ww13, ftemp13, uu14, vv14, ww14, ftemp14
      real,dimension(imax2, jmax2) :: uu15, vv15, ww15, ftemp15, uu16, vv16, ww16, ftemp16

      real,dimension(imax2, jmax2) :: uu, vv, ww, dtemp, rtemp, ctemp
      real,dimension(eimax2, ejmax2) :: fuu, fvv, fww, ftemp
      real,dimension(eimax2, ejmax2) :: ffuu, ffvv, ffww, fftemp

!c Legacy per-domain/per-corner work arrays were replaced by the
!c dimensioned uu/vv/ww/dt/rt and domain arrays declared below.

      real,dimension(kmax2) :: ldaps_u1, ldaps_v1, ldaps_u2, ldaps_v2, ldaps_u3, ldaps_v3, ldaps_u4, ldaps_v4
      real,dimension(kmax2) :: ldaps_u5, ldaps_v5, ldaps_u6, ldaps_v6, ldaps_u7, ldaps_v7, ldaps_u8, ldaps_v8
      real,dimension(kmax2) :: ldaps_u9, ldaps_v9, ldaps_u10, ldaps_v10, ldaps_u11, ldaps_v11, ldaps_u12, ldaps_v12
      real,dimension(kmax2) :: ldaps_u13, ldaps_v13, ldaps_u14, ldaps_v14, ldaps_u15, ldaps_v15, ldaps_u16, ldaps_v16
      real,dimension(kmax2) :: ldaps_uu1, ldaps_vv1, ldaps_uu2, ldaps_vv2, ldaps_uu3, ldaps_vv3, ldaps_uu4, ldaps_vv4
      real,dimension(kmax2) :: ldaps_uu5, ldaps_vv5, ldaps_uu6, ldaps_vv6, ldaps_uu7, ldaps_vv7, ldaps_uu8, ldaps_vv8
      real,dimension(kmax2) :: ldaps_uu9, ldaps_vv9, ldaps_uu10, ldaps_vv10, ldaps_uu11, ldaps_vv11, ldaps_uu12, ldaps_vv12
      real,dimension(kmax2) :: ldaps_uu13, ldaps_vv13, ldaps_uu14, ldaps_vv14, ldaps_uu15, ldaps_vv15, ldaps_uu16, ldaps_vv16
      real,dimension(kmax2) :: ldaps_t1, ldaps_t2, ldaps_t3, ldaps_t4, ldaps_t5, ldaps_t6, ldaps_t7, ldaps_t8
      real,dimension(kmax2) :: ldaps_t9, ldaps_t10, ldaps_t11, ldaps_t12, ldaps_t13, ldaps_t14, ldaps_t15, ldaps_t16
      real,dimension(kmax2) :: ldaps_tt1, ldaps_tt2, ldaps_tt3, ldaps_tt4, ldaps_tt5, ldaps_tt6, ldaps_tt7, ldaps_tt8
      real,dimension(kmax2) :: ldaps_tt9, ldaps_tt10, ldaps_tt11, ldaps_tt12, ldaps_tt13, ldaps_tt14, ldaps_tt15, ldaps_tt16
      real,dimension(kmax2,16) :: emulator_ct, emulator_mr
      real,dimension(kmax2) :: dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9, dt10, dt11, dt12, dt13, dt14, dt15, dt16

      real,dimension(kmax2) ::ldaps_ws1, ldaps_dir1, inws1_1, inws1_2, dir1
      real,dimension(kmax2) ::ldaps_ws2, ldaps_dir2, inws2_1, inws2_2, dir2
      real,dimension(kmax2) ::ldaps_ws3, ldaps_dir3, inws3_1, inws3_2, dir3
      real,dimension(kmax2) ::ldaps_ws4, ldaps_dir4, inws4_1, inws4_2, dir4
      real,dimension(kmax2) ::ldaps_ws5, ldaps_dir5, inws5_1, inws5_2, dir5
      real,dimension(kmax2) ::ldaps_ws6, ldaps_dir6, inws6_1, inws6_2, dir6
      real,dimension(kmax2) ::ldaps_ws7, ldaps_dir7, inws7_1, inws7_2, dir7
      real,dimension(kmax2) ::ldaps_ws8, ldaps_dir8, inws8_1, inws8_2, dir8
      real,dimension(kmax2) ::ldaps_ws9, ldaps_dir9, inws9_1, inws9_2, dir9
      real,dimension(kmax2) ::ldaps_ws10, ldaps_dir10, inws10_1, inws10_2, dir10
      real,dimension(kmax2) ::ldaps_ws11, ldaps_dir11, inws11_1, inws11_2, dir11
      real,dimension(kmax2) ::ldaps_ws12, ldaps_dir12, inws12_1, inws12_2, dir12
      real,dimension(kmax2) ::ldaps_ws13, ldaps_dir13, inws13_1, inws13_2, dir13
      real,dimension(kmax2) ::ldaps_ws14, ldaps_dir14, inws14_1, inws14_2, dir14
      real,dimension(kmax2) ::ldaps_ws15, ldaps_dir15, inws15_1, inws15_2, dir15
      real,dimension(kmax2) ::ldaps_ws16, ldaps_dir16, inws16_1, inws16_2, dir16
      real,dimension(kmax2) :: inflow_ws_01, inflow_ws_02, inflow_ws_03
      real,dimension(kmax2) :: inflow_ws_04, inflow_ws_05, inflow_ws_06, inflow_ws_07, inflow_ws_08

      REAL :: roof_1, road_1, green_1, soil_1, wall_1, water_1
      REAL :: roof_2, road_2, green_2, soil_2, wall_2, water_2
      REAL :: roof_3, road_3, green_3, soil_3, wall_3, water_3
      REAL :: roof_4, road_4, green_4, soil_4, wall_4, water_4
      REAL :: roof_5, road_5, green_5, soil_5, wall_5, water_5
      REAL :: roof_6, road_6, green_6, soil_6, wall_6, water_6
      REAL :: roof_7, road_7, green_7, soil_7, wall_7, water_7
      REAL :: roof_8, road_8, green_8, soil_8, wall_8, water_8
      REAL :: roof_9, road_9, green_9, soil_9, wall_9, water_9
      REAL :: roof_10, road_10, green_10, soil_10, wall_10, water_10
      REAL :: roof_11, road_11, green_11, soil_11, wall_11, water_11
      REAL :: roof_12, road_12, green_12, soil_12, wall_12, water_12
      REAL :: roof_13, road_13, green_13, soil_13, wall_13, water_13
      REAL :: roof_14, road_14, green_14, soil_14, wall_14, water_14
      REAL :: roof_15, road_15, green_15, soil_15, wall_15, water_15
      REAL :: roof_16, road_16, green_16, soil_16, wall_16, water_16

      REAL :: temp1_a, temp1_b, temp2_a, temp2_b, temp3_a, temp3_b
      REAL :: temp4_a, temp4_b, temp5_a, temp5_b, temp6_a, temp6_b
      REAL :: temp7_a, temp7_b, temp8_a, temp8_b, temp9_a, temp9_b
      REAL :: temp10_a, temp10_b, temp11_a, temp11_b, temp12_a, temp12_b
      REAL :: temp13_a, temp13_b, temp14_a, temp14_b, temp15_a, temp15_b
      REAL :: temp16_a, temp16_b

      INTEGER :: temp1_1, temp1_2, temp2_1, temp2_2, temp3_1, temp3_2
      INTEGER :: temp4_1, temp4_2, temp5_1, temp5_2, temp6_1, temp6_2
      INTEGER :: temp7_1, temp7_2, temp8_1, temp8_2, temp9_1, temp9_2
      INTEGER :: temp10_1, temp10_2, temp11_1, temp11_2, temp12_1, temp12_2
      INTEGER :: temp13_1, temp13_2, temp14_1, temp14_2, temp15_1, temp15_2
      INTEGER :: temp16_1, temp16_2 

      INTEGER :: roof_temp1_1, roof_temp1_2, roof_temp2_1, roof_temp2_2
      INTEGER :: roof_temp3_1, roof_temp3_2, roof_temp4_1, roof_temp4_2
      INTEGER :: roof_temp5_1, roof_temp5_2, roof_temp6_1, roof_temp6_2
      INTEGER :: roof_temp7_1, roof_temp7_2, roof_temp8_1, roof_temp8_2
      INTEGER :: roof_temp9_1, roof_temp9_2, roof_temp10_1, roof_temp10_2
      INTEGER :: roof_temp11_1, roof_temp11_2, roof_temp12_1, roof_temp12_2
      INTEGER :: roof_temp13_1, roof_temp13_2, roof_temp14_1, roof_temp14_2
      INTEGER :: roof_temp15_1, roof_temp15_2, roof_temp16_1, roof_temp16_2

      INTEGER :: road_temp1_1, road_temp1_2, road_temp2_1, road_temp2_2
      INTEGER :: road_temp3_1, road_temp3_2, road_temp4_1, road_temp4_2
      INTEGER :: road_temp5_1, road_temp5_2, road_temp6_1, road_temp6_2
      INTEGER :: road_temp7_1, road_temp7_2, road_temp8_1, road_temp8_2
      INTEGER :: road_temp9_1, road_temp9_2, road_temp10_1, road_temp10_2
      INTEGER :: road_temp11_1, road_temp11_2, road_temp12_1, road_temp12_2
      INTEGER :: road_temp13_1, road_temp13_2, road_temp14_1, road_temp14_2
      INTEGER :: road_temp15_1, road_temp15_2, road_temp16_1, road_temp16_2

      INTEGER :: wall_temp1_1, wall_temp1_2, wall_temp2_1, wall_temp2_2
      INTEGER :: wall_temp3_1, wall_temp3_2, wall_temp4_1, wall_temp4_2
      INTEGER :: wall_temp5_1, wall_temp5_2, wall_temp6_1, wall_temp6_2
      INTEGER :: wall_temp7_1, wall_temp7_2, wall_temp8_1, wall_temp8_2
      INTEGER :: wall_temp9_1, wall_temp9_2, wall_temp10_1, wall_temp10_2
      INTEGER :: wall_temp11_1, wall_temp11_2, wall_temp12_1, wall_temp12_2
      INTEGER :: wall_temp13_1, wall_temp13_2, wall_temp14_1, wall_temp14_2
      INTEGER :: wall_temp15_1, wall_temp15_2, wall_temp16_1, wall_temp16_2

      INTEGER :: green_temp1_1, green_temp1_2, green_temp2_1, green_temp2_2
      INTEGER :: green_temp3_1, green_temp3_2, green_temp4_1, green_temp4_2
      INTEGER :: green_temp5_1, green_temp5_2, green_temp6_1, green_temp6_2
      INTEGER :: green_temp7_1, green_temp7_2, green_temp8_1, green_temp8_2
      INTEGER :: green_temp9_1, green_temp9_2, green_temp10_1, green_temp10_2
      INTEGER :: green_temp11_1, green_temp11_2, green_temp12_1, green_temp12_2
      INTEGER :: green_temp13_1, green_temp13_2, green_temp14_1, green_temp14_2
      INTEGER :: green_temp15_1, green_temp15_2, green_temp16_1, green_temp16_2

      INTEGER :: soil_temp1_1, soil_temp1_2, soil_temp2_1, soil_temp2_2
      INTEGER :: soil_temp3_1, soil_temp3_2, soil_temp4_1, soil_temp4_2
      INTEGER :: soil_temp5_1, soil_temp5_2, soil_temp6_1, soil_temp6_2
      INTEGER :: soil_temp7_1, soil_temp7_2, soil_temp8_1, soil_temp8_2
      INTEGER :: soil_temp9_1, soil_temp9_2, soil_temp10_1, soil_temp10_2
      INTEGER :: soil_temp11_1, soil_temp11_2, soil_temp12_1, soil_temp12_2
      INTEGER :: soil_temp13_1, soil_temp13_2, soil_temp14_1, soil_temp14_2
      INTEGER :: soil_temp15_1, soil_temp15_2, soil_temp16_1, soil_temp16_2

      INTEGER :: water_temp1_1, water_temp1_2, water_temp2_1, water_temp2_2
      INTEGER :: water_temp3_1, water_temp3_2, water_temp4_1, water_temp4_2
      INTEGER :: water_temp5_1, water_temp5_2, water_temp6_1, water_temp6_2
      INTEGER :: water_temp7_1, water_temp7_2, water_temp8_1, water_temp8_2
      INTEGER :: water_temp9_1, water_temp9_2, water_temp10_1, water_temp10_2
      INTEGER :: water_temp11_1, water_temp11_2, water_temp12_1, water_temp12_2
      INTEGER :: water_temp13_1, water_temp13_2, water_temp14_1, water_temp14_2
      INTEGER :: water_temp15_1, water_temp15_2, water_temp16_1, water_temp16_2

      REAL :: roof_temp1_a, roof_temp1_b, roof_temp2_a, roof_temp2_b
      REAL :: roof_temp3_a, roof_temp3_b, roof_temp4_a, roof_temp4_b
      REAL :: roof_temp5_a, roof_temp5_b, roof_temp6_a, roof_temp6_b
      REAL :: roof_temp7_a, roof_temp7_b, roof_temp8_a, roof_temp8_b
      REAL :: roof_temp9_a, roof_temp9_b, roof_temp10_a, roof_temp10_b
      REAL :: roof_temp11_a, roof_temp11_b, roof_temp12_a, roof_temp12_b
      REAL :: roof_temp13_a, roof_temp13_b, roof_temp14_a, roof_temp14_b
      REAL :: roof_temp15_a, roof_temp15_b, roof_temp16_a, roof_temp16_b

      REAL :: road_temp1_a, road_temp1_b, road_temp2_a, road_temp2_b
      REAL :: road_temp3_a, road_temp3_b, road_temp4_a, road_temp4_b
      REAL :: road_temp5_a, road_temp5_b, road_temp6_a, road_temp6_b
      REAL :: road_temp7_a, road_temp7_b, road_temp8_a, road_temp8_b
      REAL :: road_temp9_a, road_temp9_b, road_temp10_a, road_temp10_b
      REAL :: road_temp11_a, road_temp11_b, road_temp12_a, road_temp12_b
      REAL :: road_temp13_a, road_temp13_b, road_temp14_a, road_temp14_b
      REAL :: road_temp15_a, road_temp15_b, road_temp16_a, road_temp16_b

      REAL :: wall_temp1_a, wall_temp1_b, wall_temp2_a, wall_temp2_b
      REAL :: wall_temp3_a, wall_temp3_b, wall_temp4_a, wall_temp4_b
      REAL :: wall_temp5_a, wall_temp5_b, wall_temp6_a, wall_temp6_b
      REAL :: wall_temp7_a, wall_temp7_b, wall_temp8_a, wall_temp8_b
      REAL :: wall_temp9_a, wall_temp9_b, wall_temp10_a, wall_temp10_b
      REAL :: wall_temp11_a, wall_temp11_b, wall_temp12_a, wall_temp12_b
      REAL :: wall_temp13_a, wall_temp13_b, wall_temp14_a, wall_temp14_b
      REAL :: wall_temp15_a, wall_temp15_b, wall_temp16_a, wall_temp16_b

      REAL :: green_temp1_a, green_temp1_b, green_temp2_a, green_temp2_b
      REAL :: green_temp3_a, green_temp3_b, green_temp4_a, green_temp4_b
      REAL :: green_temp5_a, green_temp5_b, green_temp6_a, green_temp6_b
      REAL :: green_temp7_a, green_temp7_b, green_temp8_a, green_temp8_b
      REAL :: green_temp9_a, green_temp9_b, green_temp10_a, green_temp10_b
      REAL :: green_temp11_a, green_temp11_b, green_temp12_a, green_temp12_b
      REAL :: green_temp13_a, green_temp13_b, green_temp14_a, green_temp14_b
      REAL :: green_temp15_a, green_temp15_b, green_temp16_a, green_temp16_b

      REAL :: soil_temp1_a, soil_temp1_b, soil_temp2_a, soil_temp2_b
      REAL :: soil_temp3_a, soil_temp3_b, soil_temp4_a, soil_temp4_b
      REAL :: soil_temp5_a, soil_temp5_b, soil_temp6_a, soil_temp6_b
      REAL :: soil_temp7_a, soil_temp7_b, soil_temp8_a, soil_temp8_b
      REAL :: soil_temp9_a, soil_temp9_b, soil_temp10_a, soil_temp10_b
      REAL :: soil_temp11_a, soil_temp11_b, soil_temp12_a, soil_temp12_b
      REAL :: soil_temp13_a, soil_temp13_b, soil_temp14_a, soil_temp14_b
      REAL :: soil_temp15_a, soil_temp15_b, soil_temp16_a, soil_temp16_b

      REAL :: water_temp1_a, water_temp1_b, water_temp2_a, water_temp2_b
      REAL :: water_temp3_a, water_temp3_b, water_temp4_a, water_temp4_b
      REAL :: water_temp5_a, water_temp5_b, water_temp6_a, water_temp6_b
      REAL :: water_temp7_a, water_temp7_b, water_temp8_a, water_temp8_b
      REAL :: water_temp9_a, water_temp9_b, water_temp10_a, water_temp10_b
      REAL :: water_temp11_a, water_temp11_b, water_temp12_a, water_temp12_b
      REAL :: water_temp13_a, water_temp13_b, water_temp14_a, water_temp14_b
      REAL :: water_temp15_a, water_temp15_b, water_temp16_a, water_temp16_b

      REAL :: ldt1_1, ldt1_2, ldt1_3, ldt1_4, ldt2_1, ldt2_2, ldt2_3, ldt2_4
      REAL :: ldt3_1, ldt3_2, ldt3_3, ldt3_4, ldt4_1, ldt4_2, ldt4_3, ldt4_4
      REAL :: ldt5_1, ldt5_2, ldt5_3, ldt5_4, ldt6_1, ldt6_2, ldt6_3, ldt6_4
      REAL :: ldt7_1, ldt7_2, ldt7_3, ldt7_4, ldt8_1, ldt8_2, ldt8_3, ldt8_4
      REAL :: ldt9_1, ldt9_2, ldt9_3, ldt9_4, ldt10_1, ldt10_2, ldt10_3, ldt10_4
      REAL :: ldt11_1, ldt11_2, ldt11_3, ldt11_4, ldt12_1, ldt12_2, ldt12_3, ldt12_4
      REAL :: ldt13_1, ldt13_2, ldt13_3, ldt13_4, ldt14_1, ldt14_2, ldt14_3, ldt14_4
      REAL :: ldt15_1, ldt15_2, ldt15_3, ldt15_4, ldt16_1, ldt16_2, ldt16_3, ldt16_4
      
      REAL :: ltemp1, ltemp2, ltemp3, ltemp4, ltemp5, ltemp6, ltemp7, ltemp8, ltemp9
      REAL :: ltemp10, ltemp11, ltemp12, ltemp13, ltemp14, ltemp15, ltemp16

      REAL :: wyS, wyN, wxW, wxE
      REAL, DIMENSION(16) :: domain_bg_u, domain_bg_v, domain_bg_t, domain_ws
      REAL, DIMENSION(16) :: domain_met_dir, domain_cfd_dir, domain_alpha
      REAL, DIMENSION(16) :: ws_lower_value, ws_upper_value
      REAL, DIMENSION(16) :: ws_delta_lower, ws_delta_upper
      REAL, DIMENSION(16) :: wd_weight_lower, wd_weight_upper
      INTEGER, DIMENSION(16,2) :: cfd_ws_case, cfd_angle_case
      REAL, DIMENSION(8) :: inflow_at_level
      INTEGER :: wind_domain, wind_case
      real,dimension(32) :: dir1_2, dir2_2, dir3_2, dir4_2
      real,dimension(32) :: dir5_2, dir6_2, dir7_2, dir8_2
      real,dimension(32) :: dir9_2, dir10_2, dir11_2, dir12_2
      real,dimension(32) :: dir13_2, dir14_2, dir15_2, dir16_2
      INTEGER :: sc_yy, sc_mm, sc_dd, sc_hh
      INTEGER :: arg_status, date_ios, days_in_month
      INTEGER :: io_unit, domain_idx, surface_idx, case_idx
      INTEGER :: output_ios
      LOGICAL :: leap_year
      CHARACTER(10) :: datetime_arg

      CHARACTER(999) :: ofile, output_iomsg

      !D01 DB 
      CHARACTER(999) :: ifile111,ifile112,ifile113,ifile114,ifile115,ifile116,ifile117,ifile118
      CHARACTER(999) :: ifile121,ifile122,ifile123,ifile124,ifile125,ifile126,ifile127,ifile128
      CHARACTER(999) :: ifile131,ifile132,ifile133,ifile134,ifile135,ifile136,ifile137,ifile138
      CHARACTER(999) :: ifile141,ifile142,ifile143,ifile144,ifile145,ifile146,ifile147,ifile148
      CHARACTER(999) :: ifile151,ifile152,ifile153,ifile154,ifile155,ifile156,ifile157,ifile158
      CHARACTER(999) :: ifile161,ifile162,ifile163,ifile164,ifile165,ifile166,ifile167,ifile168
      !D02 DB 
      CHARACTER(999) :: ifile211,ifile212,ifile213,ifile214,ifile215,ifile216,ifile217,ifile218
      CHARACTER(999) :: ifile221,ifile222,ifile223,ifile224,ifile225,ifile226,ifile227,ifile228
      CHARACTER(999) :: ifile231,ifile232,ifile233,ifile234,ifile235,ifile236,ifile237,ifile238
      CHARACTER(999) :: ifile241,ifile242,ifile243,ifile244,ifile245,ifile246,ifile247,ifile248
      CHARACTER(999) :: ifile251,ifile252,ifile253,ifile254,ifile255,ifile256,ifile257,ifile258
      CHARACTER(999) :: ifile261,ifile262,ifile263,ifile264,ifile265,ifile266,ifile267,ifile268
      !D03 DB 
      CHARACTER(999) :: ifile311,ifile312,ifile313,ifile314,ifile315,ifile316,ifile317,ifile318
      CHARACTER(999) :: ifile321,ifile322,ifile323,ifile324,ifile325,ifile326,ifile327,ifile328
      CHARACTER(999) :: ifile331,ifile332,ifile333,ifile334,ifile335,ifile336,ifile337,ifile338
      CHARACTER(999) :: ifile341,ifile342,ifile343,ifile344,ifile345,ifile346,ifile347,ifile348
      CHARACTER(999) :: ifile351,ifile352,ifile353,ifile354,ifile355,ifile356,ifile357,ifile358
      CHARACTER(999) :: ifile361,ifile362,ifile363,ifile364,ifile365,ifile366,ifile367,ifile368
      !D04 DB 
      CHARACTER(999) :: ifile411,ifile412,ifile413,ifile414,ifile415,ifile416,ifile417,ifile418
      CHARACTER(999) :: ifile421,ifile422,ifile423,ifile424,ifile425,ifile426,ifile427,ifile428
      CHARACTER(999) :: ifile431,ifile432,ifile433,ifile434,ifile435,ifile436,ifile437,ifile438
      CHARACTER(999) :: ifile441,ifile442,ifile443,ifile444,ifile445,ifile446,ifile447,ifile448
      CHARACTER(999) :: ifile451,ifile452,ifile453,ifile454,ifile455,ifile456,ifile457,ifile458
      CHARACTER(999) :: ifile461,ifile462,ifile463,ifile464,ifile465,ifile466,ifile467,ifile468
      !D05 DB 
      CHARACTER(999) :: ifile511,ifile512,ifile513,ifile514,ifile515,ifile516,ifile517,ifile518
      CHARACTER(999) :: ifile521,ifile522,ifile523,ifile524,ifile525,ifile526,ifile527,ifile528
      CHARACTER(999) :: ifile531,ifile532,ifile533,ifile534,ifile535,ifile536,ifile537,ifile538
      CHARACTER(999) :: ifile541,ifile542,ifile543,ifile544,ifile545,ifile546,ifile547,ifile548
      CHARACTER(999) :: ifile551,ifile552,ifile553,ifile554,ifile555,ifile556,ifile557,ifile558
      CHARACTER(999) :: ifile561,ifile562,ifile563,ifile564,ifile565,ifile566,ifile567,ifile568
      !D06 DB 
      CHARACTER(999) :: ifile611,ifile612,ifile613,ifile614,ifile615,ifile616,ifile617,ifile618
      CHARACTER(999) :: ifile621,ifile622,ifile623,ifile624,ifile625,ifile626,ifile627,ifile628
      CHARACTER(999) :: ifile631,ifile632,ifile633,ifile634,ifile635,ifile636,ifile637,ifile638
      CHARACTER(999) :: ifile641,ifile642,ifile643,ifile644,ifile645,ifile646,ifile647,ifile648
      CHARACTER(999) :: ifile651,ifile652,ifile653,ifile654,ifile655,ifile656,ifile657,ifile658
      CHARACTER(999) :: ifile661,ifile662,ifile663,ifile664,ifile665,ifile666,ifile667,ifile668
      !D07 DB 
      CHARACTER(999) :: ifile711,ifile712,ifile713,ifile714,ifile715,ifile716,ifile717,ifile718
      CHARACTER(999) :: ifile721,ifile722,ifile723,ifile724,ifile725,ifile726,ifile727,ifile728
      CHARACTER(999) :: ifile731,ifile732,ifile733,ifile734,ifile735,ifile736,ifile737,ifile738
      CHARACTER(999) :: ifile741,ifile742,ifile743,ifile744,ifile745,ifile746,ifile747,ifile748
      CHARACTER(999) :: ifile751,ifile752,ifile753,ifile754,ifile755,ifile756,ifile757,ifile758
      CHARACTER(999) :: ifile761,ifile762,ifile763,ifile764,ifile765,ifile766,ifile767,ifile768
      !D08 DB 
      CHARACTER(999) :: ifile811,ifile812,ifile813,ifile814,ifile815,ifile816,ifile817,ifile818
      CHARACTER(999) :: ifile821,ifile822,ifile823,ifile824,ifile825,ifile826,ifile827,ifile828
      CHARACTER(999) :: ifile831,ifile832,ifile833,ifile834,ifile835,ifile836,ifile837,ifile838
      CHARACTER(999) :: ifile841,ifile842,ifile843,ifile844,ifile845,ifile846,ifile847,ifile848
      CHARACTER(999) :: ifile851,ifile852,ifile853,ifile854,ifile855,ifile856,ifile857,ifile858
      CHARACTER(999) :: ifile861,ifile862,ifile863,ifile864,ifile865,ifile866,ifile867,ifile868
      !D09 DB 
      CHARACTER(999) :: ifile911,ifile912,ifile913,ifile914,ifile915,ifile916,ifile917,ifile918
      CHARACTER(999) :: ifile921,ifile922,ifile923,ifile924,ifile925,ifile926,ifile927,ifile928
      CHARACTER(999) :: ifile931,ifile932,ifile933,ifile934,ifile935,ifile936,ifile937,ifile938
      CHARACTER(999) :: ifile941,ifile942,ifile943,ifile944,ifile945,ifile946,ifile947,ifile948
      CHARACTER(999) :: ifile951,ifile952,ifile953,ifile954,ifile955,ifile956,ifile957,ifile958
      CHARACTER(999) :: ifile961,ifile962,ifile963,ifile964,ifile965,ifile966,ifile967,ifile968
      !D10 DB 
      CHARACTER(999) :: ifile1011,ifile1012,ifile1013,ifile1014,ifile1015,ifile1016,ifile1017,ifile1018
      CHARACTER(999) :: ifile1021,ifile1022,ifile1023,ifile1024,ifile1025,ifile1026,ifile1027,ifile1028
      CHARACTER(999) :: ifile1031,ifile1032,ifile1033,ifile1034,ifile1035,ifile1036,ifile1037,ifile1038
      CHARACTER(999) :: ifile1041,ifile1042,ifile1043,ifile1044,ifile1045,ifile1046,ifile1047,ifile1048
      CHARACTER(999) :: ifile1051,ifile1052,ifile1053,ifile1054,ifile1055,ifile1056,ifile1057,ifile1058
      CHARACTER(999) :: ifile1061,ifile1062,ifile1063,ifile1064,ifile1065,ifile1066,ifile1067,ifile1068
      !D11 DB 
      CHARACTER(999) :: ifile1111,ifile1112,ifile1113,ifile1114,ifile1115,ifile1116,ifile1117,ifile1118
      CHARACTER(999) :: ifile1121,ifile1122,ifile1123,ifile1124,ifile1125,ifile1126,ifile1127,ifile1128
      CHARACTER(999) :: ifile1131,ifile1132,ifile1133,ifile1134,ifile1135,ifile1136,ifile1137,ifile1138
      CHARACTER(999) :: ifile1141,ifile1142,ifile1143,ifile1144,ifile1145,ifile1146,ifile1147,ifile1148
      CHARACTER(999) :: ifile1151,ifile1152,ifile1153,ifile1154,ifile1155,ifile1156,ifile1157,ifile1158
      CHARACTER(999) :: ifile1161,ifile1162,ifile1163,ifile1164,ifile1165,ifile1166,ifile1167,ifile1168
      !D12 DB 
      CHARACTER(999) :: ifile1211,ifile1212,ifile1213,ifile1214,ifile1215,ifile1216,ifile1217,ifile1218
      CHARACTER(999) :: ifile1221,ifile1222,ifile1223,ifile1224,ifile1225,ifile1226,ifile1227,ifile1228
      CHARACTER(999) :: ifile1231,ifile1232,ifile1233,ifile1234,ifile1235,ifile1236,ifile1237,ifile1238
      CHARACTER(999) :: ifile1241,ifile1242,ifile1243,ifile1244,ifile1245,ifile1246,ifile1247,ifile1248
      CHARACTER(999) :: ifile1251,ifile1252,ifile1253,ifile1254,ifile1255,ifile1256,ifile1257,ifile1258
      CHARACTER(999) :: ifile1261,ifile1262,ifile1263,ifile1264,ifile1265,ifile1266,ifile1267,ifile1268
      !D13 DB 
      CHARACTER(999) :: ifile1311,ifile1312,ifile1313,ifile1314,ifile1315,ifile1316,ifile1317,ifile1318
      CHARACTER(999) :: ifile1321,ifile1322,ifile1323,ifile1324,ifile1325,ifile1326,ifile1327,ifile1328
      CHARACTER(999) :: ifile1331,ifile1332,ifile1333,ifile1334,ifile1335,ifile1336,ifile1337,ifile1338
      CHARACTER(999) :: ifile1341,ifile1342,ifile1343,ifile1344,ifile1345,ifile1346,ifile1347,ifile1348
      CHARACTER(999) :: ifile1351,ifile1352,ifile1353,ifile1354,ifile1355,ifile1356,ifile1357,ifile1358
      CHARACTER(999) :: ifile1361,ifile1362,ifile1363,ifile1364,ifile1365,ifile1366,ifile1367,ifile1368
      !D14 DB 
      CHARACTER(999) :: ifile1411,ifile1412,ifile1413,ifile1414,ifile1415,ifile1416,ifile1417,ifile1418
      CHARACTER(999) :: ifile1421,ifile1422,ifile1423,ifile1424,ifile1425,ifile1426,ifile1427,ifile1428
      CHARACTER(999) :: ifile1431,ifile1432,ifile1433,ifile1434,ifile1435,ifile1436,ifile1437,ifile1438
      CHARACTER(999) :: ifile1441,ifile1442,ifile1443,ifile1444,ifile1445,ifile1446,ifile1447,ifile1448
      CHARACTER(999) :: ifile1451,ifile1452,ifile1453,ifile1454,ifile1455,ifile1456,ifile1457,ifile1458
      CHARACTER(999) :: ifile1461,ifile1462,ifile1463,ifile1464,ifile1465,ifile1466,ifile1467,ifile1468
      !D15 DB 
      CHARACTER(999) :: ifile1511,ifile1512,ifile1513,ifile1514,ifile1515,ifile1516,ifile1517,ifile1518
      CHARACTER(999) :: ifile1521,ifile1522,ifile1523,ifile1524,ifile1525,ifile1526,ifile1527,ifile1528
      CHARACTER(999) :: ifile1531,ifile1532,ifile1533,ifile1534,ifile1535,ifile1536,ifile1537,ifile1538
      CHARACTER(999) :: ifile1541,ifile1542,ifile1543,ifile1544,ifile1545,ifile1546,ifile1547,ifile1548
      CHARACTER(999) :: ifile1551,ifile1552,ifile1553,ifile1554,ifile1555,ifile1556,ifile1557,ifile1558
      CHARACTER(999) :: ifile1561,ifile1562,ifile1563,ifile1564,ifile1565,ifile1566,ifile1567,ifile1568
      !D16 DB 
      CHARACTER(999) :: ifile1611,ifile1612,ifile1613,ifile1614,ifile1615,ifile1616,ifile1617,ifile1618
      CHARACTER(999) :: ifile1621,ifile1622,ifile1623,ifile1624,ifile1625,ifile1626,ifile1627,ifile1628
      CHARACTER(999) :: ifile1631,ifile1632,ifile1633,ifile1634,ifile1635,ifile1636,ifile1637,ifile1638
      CHARACTER(999) :: ifile1641,ifile1642,ifile1643,ifile1644,ifile1645,ifile1646,ifile1647,ifile1648
      CHARACTER(999) :: ifile1651,ifile1652,ifile1653,ifile1654,ifile1655,ifile1656,ifile1657,ifile1658
      CHARACTER(999) :: ifile1661,ifile1662,ifile1663,ifile1664,ifile1665,ifile1666,ifile1667,ifile1668

      !LDAPS Profile
      CHARACTER(999) :: l_in_u1, l_in_v1, l_in_u2, l_in_v2, l_in_u3, l_in_v3, l_in_u4, l_in_v4
      CHARACTER(999) :: l_in_u5, l_in_v5, l_in_u6, l_in_v6, l_in_u7, l_in_v7, l_in_u8, l_in_v8
      CHARACTER(999) :: l_in_u9, l_in_v9, l_in_u10, l_in_v10, l_in_u11, l_in_v11, l_in_u12, l_in_v12
      CHARACTER(999) :: l_in_u13, l_in_v13, l_in_u14, l_in_v14, l_in_u15, l_in_v15, l_in_u16, l_in_v16
      CHARACTER(999) :: l_in_t1, l_in_t2, l_in_t3, l_in_t4, l_in_t5, l_in_t6, l_in_t7, l_in_t8
      CHARACTER(999) :: l_in_t9, l_in_t10, l_in_t11, l_in_t12, l_in_t13, l_in_t14, l_in_t15, l_in_t16
      REAL, DIMENSION(kmax2,16) :: ldaps_input_u, ldaps_input_v, ldaps_input_t
      CHARACTER(999) :: ldaps_u_file, ldaps_v_file, ldaps_t_file
      INTEGER :: ldaps_domain

      !ML emulator profiles
      CHARACTER(999) :: emulator_ct_file, emulator_mr_file
      INTEGER :: emulator_domain
      REAL, DIMENSION(16,6) :: surface_ratio
      REAL, DIMENSION(16,6) :: surface_delta_lower, surface_delta_upper
      INTEGER, DIMENSION(16,6,2) :: surface_class_case
      REAL, DIMENSION(5,6) :: surface_class_value
      CHARACTER(999) :: rsa_file
      INTEGER :: surface_domain, surface_type, surface_class

!c Unified D01-D16 database input and interpolation work arrays.
!c The U, V, W, and temperature fields remain separate variables.
      REAL, DIMENSION(imax2,jmax2) :: uu_db1, vv_db1, ww_db1, dt_db1, rt_db1
      REAL, DIMENSION(imax2,jmax2) :: uu_db2, vv_db2, ww_db2, dt_db2, rt_db2
      REAL, DIMENSION(imax2,jmax2) :: uu_db3, vv_db3, ww_db3, dt_db3, rt_db3
      REAL, DIMENSION(imax2,jmax2) :: uu_db4, vv_db4, ww_db4, dt_db4, rt_db4
      REAL, DIMENSION(imax2,jmax2) :: uu_db5, vv_db5, ww_db5, dt_db5, rt_db5
      REAL, DIMENSION(imax2,jmax2) :: uu_db6, vv_db6, ww_db6, dt_db6, rt_db6
      REAL, DIMENSION(imax2,jmax2) :: uu_db7, vv_db7, ww_db7, dt_db7, rt_db7
      REAL, DIMENSION(imax2,jmax2) :: uu_db8, vv_db8, ww_db8, dt_db8, rt_db8
      REAL, DIMENSION(imax2,jmax2,16) :: uu_domain, vv_domain
      REAL, DIMENSION(imax2,jmax2,16) :: ww_domain, ftemp_domain
      REAL, DIMENSION(imax2,jmax2) :: u_interp, v_interp, w_interp
      REAL, DIMENSION(imax2,jmax2) :: dtemp_interp, rtemp_interp
      REAL, DIMENSION(2) :: surface_weight, speed_weight, angle_weight
      REAL, DIMENSION(8) :: corner_weight
      CHARACTER(5), DIMENSION(6) :: surface_name
      CHARACTER(999) :: db_file
      INTEGER :: db_domain, db_surface, db_corner, db_unit
      INTEGER :: class_slot, speed_slot, angle_slot
      REAL :: velocity_scale, weight_sum

      !RSA
      CHARACTER(999) :: l_in_f1, l_in_f2, l_in_f3, l_in_f4, l_in_f5, l_in_f6, l_in_f7, l_in_f8
      CHARACTER(999) :: l_in_f9, l_in_f10, l_in_f11, l_in_f12, l_in_f13, l_in_f14, l_in_f15, l_in_f16

!c############################################################################
!c fixed test time: 2021-07-04 22 UTC
!c############################################################################
      datetime_arg = '2021070422'
      sc_yy = 2021
      sc_mm = 7
      sc_dd = 4
      sc_hh = 22

      leap_year = (MOD(sc_yy, 400) .EQ. 0) .OR. &
                  (MOD(sc_yy, 4) .EQ. 0 .AND. MOD(sc_yy, 100) .NE. 0)
      SELECT CASE (sc_mm)
      CASE (1, 3, 5, 7, 8, 10, 12)
        days_in_month = 31
      CASE (4, 6, 9, 11)
        days_in_month = 30
      CASE (2)
        days_in_month = 28
        IF (leap_year) days_in_month = 29
      END SELECT

      IF (sc_dd .GT. days_in_month) THEN
        WRITE(*,'(A)') 'Error: date does not exist in the calendar.'
        STOP 1
      END IF

      WRITE(*,'(A,1X,A)') 'e_SOFT CUBE target:', datetime_arg

!c############################################################################
!c ML emulator profiles: cT for temperature and Mr for momentum
!c############################################################################
      DO emulator_domain = 1, 16
        WRITE(emulator_ct_file, &
             '(A,I2.2,A,I4.4,I2.2,I2.2,I2.2,A)') &
             './data/Input/Emulator/', emulator_domain, '/cT_', &
             sc_yy, sc_mm, sc_dd, sc_hh, '.txt'
        WRITE(emulator_mr_file, &
             '(A,I2.2,A,I4.4,I2.2,I2.2,I2.2,A)') &
             './data/Input/Emulator/', emulator_domain, '/Mr_', &
             sc_yy, sc_mm, sc_dd, sc_hh, '.txt'

        OPEN(101, file=emulator_ct_file, status='old')
        OPEN(102, file=emulator_mr_file, status='old')
        DO k = 1, kmax2
          READ(101,*) emulator_ct(k,emulator_domain)
          READ(102,*) emulator_mr(k,emulator_domain)
        END DO
        CLOSE(101)
        CLOSE(102)
      END DO

!c#########################################################################
open(11,file='./data/Input/Beaufort/Inflow_01.txt',status='old')
open(12,file='./data/Input/Beaufort/Inflow_02.txt',status='old')
open(13,file='./data/Input/Beaufort/Inflow_03.txt',status='old')
open(14,file='./data/Input/Beaufort/Inflow_04.txt',status='old')
open(15,file='./data/Input/Beaufort/Inflow_05.txt',status='old')
open(16,file='./data/Input/Beaufort/Inflow_06.txt',status='old')
open(17,file='./data/Input/Beaufort/Inflow_07.txt',status='old')
open(18,file='./data/Input/Beaufort/Inflow_08.txt',status='old')

!c############################################################################
!c  LDAPS or AWS 
!c############################################################################

     do 7000 k = 1, kmax2
     READ(11,*) inflow_ws_01(k)
     READ(12,*) inflow_ws_02(k)
     READ(13,*) inflow_ws_03(k)
     READ(14,*) inflow_ws_04(k)
     READ(15,*) inflow_ws_05(k)
     READ(16,*) inflow_ws_06(k)
     READ(17,*) inflow_ws_07(k)
     READ(18,*) inflow_ws_08(k)
 7000 continue

     DO io_unit = 11, 18
       CLOSE(io_unit)
     END DO

  770 format(A,I4,I2.2,I2.2,A,I2.2,A)
  777 format(A,I4.4,I2.2,I2.2,I2.2,A)

!!!!!!!!!!!!!!!!!!!! RSA !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     surface_class_value = RESHAPE([ &
          0.970,0.995,1.020,1.045,1.070, & ! Roof
          0.980,1.000,1.020,1.040,1.060, & ! Wall
          0.980,1.000,1.020,1.040,1.060, & ! Road
          0.960,0.980,1.000,1.020,1.040, & ! Green
          0.970,0.990,1.010,1.030,1.050, & ! Soil
          0.950,0.990,1.030,1.070,1.110  & ! Water
          ], [5,6])

     DO surface_domain = 1, 16
       WRITE(rsa_file,'(A,I2.2,A,I4.4,I2.2,I2.2,I2.2,A)') &
            './data/Input/RSA/',surface_domain,'/RSA_', &
            sc_yy,sc_mm,sc_dd,sc_hh,'.txt'
       OPEN(21,file=TRIM(rsa_file),status='old')
       READ(21,*) surface_ratio(surface_domain,1), & ! Roof
                  surface_ratio(surface_domain,3), & ! Road
                  surface_ratio(surface_domain,4), & ! Green
                  surface_ratio(surface_domain,5), & ! Soil
                  surface_ratio(surface_domain,2), & ! Wall
                  surface_ratio(surface_domain,6)    ! Water
       CLOSE(21)
     END DO

     DO surface_domain = 1, 16
       DO surface_type = 1, 6
         IF (surface_ratio(surface_domain,surface_type) < &
             surface_class_value(1,surface_type)) THEN
           surface_class_case(surface_domain,surface_type,:) = [1,1]
           surface_delta_lower(surface_domain,surface_type) = 0.0
           surface_delta_upper(surface_domain,surface_type) = 0.0
         ELSE IF (surface_ratio(surface_domain,surface_type) > &
                  surface_class_value(5,surface_type)) THEN
           surface_class_case(surface_domain,surface_type,:) = [5,5]
           surface_delta_lower(surface_domain,surface_type) = 0.0
           surface_delta_upper(surface_domain,surface_type) = 0.0
         ELSE
           DO surface_class = 1, 4
             IF (surface_ratio(surface_domain,surface_type) >= &
                 surface_class_value(surface_class,surface_type) .AND. &
                 surface_ratio(surface_domain,surface_type) <= &
                 surface_class_value(surface_class+1,surface_type)) THEN
               surface_class_case(surface_domain,surface_type,:) = &
                    [surface_class,surface_class+1]
               surface_delta_lower(surface_domain,surface_type) = &
                    surface_ratio(surface_domain,surface_type) - &
                    surface_class_value(surface_class,surface_type)
               surface_delta_upper(surface_domain,surface_type) = &
                    surface_class_value(surface_class+1,surface_type) - &
                    surface_ratio(surface_domain,surface_type)
               EXIT
             END IF
           END DO
         END IF
       END DO
     END DO
!!!!!!!!!!!!!!!!!!!! LDAPS Profile (Etas)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     DO ldaps_domain = 1, 16
       WRITE(ldaps_u_file,'(A,I2.2,A,I4.4,I2.2,I2.2,I2.2,A)') &
            './data/Input/LDAPS/',ldaps_domain,'/U_', &
            sc_yy,sc_mm,sc_dd,sc_hh,'.txt'
       WRITE(ldaps_v_file,'(A,I2.2,A,I4.4,I2.2,I2.2,I2.2,A)') &
            './data/Input/LDAPS/',ldaps_domain,'/V_', &
            sc_yy,sc_mm,sc_dd,sc_hh,'.txt'
       WRITE(ldaps_t_file,'(A,I2.2,A,I4.4,I2.2,I2.2,I2.2,A)') &
            './data/Input/LDAPS/',ldaps_domain,'/T_', &
            sc_yy,sc_mm,sc_dd,sc_hh,'.txt'
       OPEN(41,file=TRIM(ldaps_u_file),status='old')
       OPEN(42,file=TRIM(ldaps_v_file),status='old')
       OPEN(43,file=TRIM(ldaps_t_file),status='old')
       DO k = 1, kmax2
         READ(41,*) ldaps_input_u(k,ldaps_domain)
         READ(42,*) ldaps_input_v(k,ldaps_domain)
         READ(43,*) ldaps_input_t(k,ldaps_domain)
       END DO
       CLOSE(41)
       CLOSE(42)
       CLOSE(43)
     END DO
     ldaps_uu1 = ldaps_input_u(:,1)
     ldaps_uu2 = ldaps_input_u(:,2)
     ldaps_uu3 = ldaps_input_u(:,3)
     ldaps_uu4 = ldaps_input_u(:,4)
     ldaps_uu5 = ldaps_input_u(:,5)
     ldaps_uu6 = ldaps_input_u(:,6)
     ldaps_uu7 = ldaps_input_u(:,7)
     ldaps_uu8 = ldaps_input_u(:,8)
     ldaps_uu9 = ldaps_input_u(:,9)
     ldaps_uu10 = ldaps_input_u(:,10)
     ldaps_uu11 = ldaps_input_u(:,11)
     ldaps_uu12 = ldaps_input_u(:,12)
     ldaps_uu13 = ldaps_input_u(:,13)
     ldaps_uu14 = ldaps_input_u(:,14)
     ldaps_uu15 = ldaps_input_u(:,15)
     ldaps_uu16 = ldaps_input_u(:,16)
     ldaps_vv1 = ldaps_input_v(:,1)
     ldaps_vv2 = ldaps_input_v(:,2)
     ldaps_vv3 = ldaps_input_v(:,3)
     ldaps_vv4 = ldaps_input_v(:,4)
     ldaps_vv5 = ldaps_input_v(:,5)
     ldaps_vv6 = ldaps_input_v(:,6)
     ldaps_vv7 = ldaps_input_v(:,7)
     ldaps_vv8 = ldaps_input_v(:,8)
     ldaps_vv9 = ldaps_input_v(:,9)
     ldaps_vv10 = ldaps_input_v(:,10)
     ldaps_vv11 = ldaps_input_v(:,11)
     ldaps_vv12 = ldaps_input_v(:,12)
     ldaps_vv13 = ldaps_input_v(:,13)
     ldaps_vv14 = ldaps_input_v(:,14)
     ldaps_vv15 = ldaps_input_v(:,15)
     ldaps_vv16 = ldaps_input_v(:,16)
     ldaps_tt1 = ldaps_input_t(:,1)
     ldaps_tt2 = ldaps_input_t(:,2)
     ldaps_tt3 = ldaps_input_t(:,3)
     ldaps_tt4 = ldaps_input_t(:,4)
     ldaps_tt5 = ldaps_input_t(:,5)
     ldaps_tt6 = ldaps_input_t(:,6)
     ldaps_tt7 = ldaps_input_t(:,7)
     ldaps_tt8 = ldaps_input_t(:,8)
     ldaps_tt9 = ldaps_input_t(:,9)
     ldaps_tt10 = ldaps_input_t(:,10)
     ldaps_tt11 = ldaps_input_t(:,11)
     ldaps_tt12 = ldaps_input_t(:,12)
     ldaps_tt13 = ldaps_input_t(:,13)
     ldaps_tt14 = ldaps_input_t(:,14)
     ldaps_tt15 = ldaps_input_t(:,15)
     ldaps_tt16 = ldaps_input_t(:,16)

     pi=atan(1.)*4.

     DO k = 1, kmax2
     ldaps_u1(k) = ldaps_uu1(k)
     ldaps_u2(k) = ldaps_uu2(k)
     ldaps_u3(k) = ldaps_uu3(k)
     ldaps_u4(k) = ldaps_uu4(k)
     ldaps_u5(k) = ldaps_uu5(k)
     ldaps_u6(k) = ldaps_uu6(k)
     ldaps_u7(k) = ldaps_uu7(k)
     ldaps_u8(k) = ldaps_uu8(k)
     ldaps_u9(k) = ldaps_uu9(k)
     ldaps_u10(k) = ldaps_uu10(k)
     ldaps_u11(k) = ldaps_uu11(k)
     ldaps_u12(k) = ldaps_uu12(k)
     ldaps_u13(k) = ldaps_uu13(k)
     ldaps_u14(k) = ldaps_uu14(k)
     ldaps_u15(k) = ldaps_uu15(k)
     ldaps_u16(k) = ldaps_uu16(k)
     ldaps_v1(k) = ldaps_vv1(k)
     ldaps_v2(k) = ldaps_vv2(k)
     ldaps_v3(k) = ldaps_vv3(k)
     ldaps_v4(k) = ldaps_vv4(k)
     ldaps_v5(k) = ldaps_vv5(k)
     ldaps_v6(k) = ldaps_vv6(k)
     ldaps_v7(k) = ldaps_vv7(k)
     ldaps_v8(k) = ldaps_vv8(k)
     ldaps_v9(k) = ldaps_vv9(k)
     ldaps_v10(k) = ldaps_vv10(k)
     ldaps_v11(k) = ldaps_vv11(k)
     ldaps_v12(k) = ldaps_vv12(k)
     ldaps_v13(k) = ldaps_vv13(k)
     ldaps_v14(k) = ldaps_vv14(k)
     ldaps_v15(k) = ldaps_vv15(k)
     ldaps_v16(k) = ldaps_vv16(k)
     ldaps_t1(k) = ldaps_tt1(k) + 273.15
     ldaps_t2(k) = ldaps_tt2(k) + 273.15
     ldaps_t3(k) = ldaps_tt3(k) + 273.15
     ldaps_t4(k) = ldaps_tt4(k) + 273.15
     ldaps_t5(k) = ldaps_tt5(k) + 273.15
     ldaps_t6(k) = ldaps_tt6(k) + 273.15
     ldaps_t7(k) = ldaps_tt7(k) + 273.15
     ldaps_t8(k) = ldaps_tt8(k) + 273.15
     ldaps_t9(k) = ldaps_tt9(k) + 273.15
     ldaps_t10(k) = ldaps_tt10(k) + 273.15
     ldaps_t11(k) = ldaps_tt11(k) + 273.15
     ldaps_t12(k) = ldaps_tt12(k) + 273.15
     ldaps_t13(k) = ldaps_tt13(k) + 273.15
     ldaps_t14(k) = ldaps_tt14(k) + 273.15
     ldaps_t15(k) = ldaps_tt15(k) + 273.15
     ldaps_t16(k) = ldaps_tt16(k) + 273.15


      IF (ldaps_v1(k).GT.0.) THEN
      ldaps_dir1(k) = ((180./pi)*atan(ldaps_u1(k)/ldaps_v1(k)))+180.
      ELSE IF (ldaps_u1(k) .LT. 0. .AND. ldaps_v1(k).LT.0.) THEN
      ldaps_dir1(k) = ((180./pi)*atan(ldaps_u1(k)/ldaps_v1(k)))+0.
      ELSE IF (ldaps_u1(k) .GT. 0. .AND. ldaps_v1(k).LT.0.) THEN
      ldaps_dir1(k) = ((180./pi)*atan(ldaps_u1(k)/ldaps_v1(k)))+360.
      ELSE
      ldaps_dir1(k) = 0.
      END IF
      ldaps_ws1(k) = SQRT(ldaps_u1(k)**2.0 + ldaps_v1(k)**2.0)

      IF (ldaps_v2(k).GT.0.) THEN
      ldaps_dir2(k) = ((180./pi)*atan(ldaps_u2(k)/ldaps_v2(k)))+180.
      ELSE IF (ldaps_u2(k) .LT. 0. .AND. ldaps_v2(k).LT.0.) THEN
      ldaps_dir2(k) = ((180./pi)*atan(ldaps_u2(k)/ldaps_v2(k)))+0.
      ELSE IF (ldaps_u2(k) .GT. 0. .AND. ldaps_v2(k).LT.0.) THEN
      ldaps_dir2(k) = ((180./pi)*atan(ldaps_u2(k)/ldaps_v2(k)))+360.
      ELSE
      ldaps_dir2(k) = 0.
      END IF
      ldaps_ws2(k) = SQRT(ldaps_u2(k)**2.0 + ldaps_v2(k)**2.0)

      IF (ldaps_v3(k).GT.0.) THEN
      ldaps_dir3(k) = ((180./pi)*atan(ldaps_u3(k)/ldaps_v3(k)))+180.
      ELSE IF (ldaps_u3(k) .LT. 0. .AND. ldaps_v3(k).LT.0.) THEN
      ldaps_dir3(k) = ((180./pi)*atan(ldaps_u3(k)/ldaps_v3(k)))+0.
      ELSE IF (ldaps_u3(k) .GT. 0. .AND. ldaps_v3(k).LT.0.) THEN
      ldaps_dir3(k) = ((180./pi)*atan(ldaps_u3(k)/ldaps_v3(k)))+360.
      ELSE
      ldaps_dir3(k) = 0.
      END IF
      ldaps_ws3(k) = SQRT(ldaps_u3(k)**2.0 + ldaps_v3(k)**2.0)

      IF (ldaps_v4(k).GT.0.) THEN
      ldaps_dir4(k) = ((180./pi)*atan(ldaps_u4(k)/ldaps_v4(k)))+180.
      ELSE IF (ldaps_u4(k) .LT. 0. .AND. ldaps_v4(k).LT.0.) THEN
      ldaps_dir4(k) = ((180./pi)*atan(ldaps_u4(k)/ldaps_v4(k)))+0.
      ELSE IF (ldaps_u4(k) .GT. 0. .AND. ldaps_v4(k).LT.0.) THEN
      ldaps_dir4(k) = ((180./pi)*atan(ldaps_u4(k)/ldaps_v4(k)))+360.
      ELSE
      ldaps_dir4(k) = 0.
      END IF
      ldaps_ws4(k) = SQRT(ldaps_u4(k)**2.0 + ldaps_v4(k)**2.0)

      IF (ldaps_v5(k).GT.0.) THEN
      ldaps_dir5(k) = ((180./pi)*atan(ldaps_u5(k)/ldaps_v5(k)))+180.
      ELSE IF (ldaps_u5(k) .LT. 0. .AND. ldaps_v5(k).LT.0.) THEN
      ldaps_dir5(k) = ((180./pi)*atan(ldaps_u5(k)/ldaps_v5(k)))+0.
      ELSE IF (ldaps_u5(k) .GT. 0. .AND. ldaps_v5(k).LT.0.) THEN
      ldaps_dir5(k) = ((180./pi)*atan(ldaps_u5(k)/ldaps_v5(k)))+360.
      ELSE
      ldaps_dir5(k) = 0.
      END IF
      ldaps_ws5(k) = SQRT(ldaps_u5(k)**2.0 + ldaps_v5(k)**2.0)

      IF (ldaps_v6(k).GT.0.) THEN
      ldaps_dir6(k) = ((180./pi)*atan(ldaps_u6(k)/ldaps_v6(k)))+180.
      ELSE IF (ldaps_u6(k) .LT. 0. .AND. ldaps_v6(k).LT.0.) THEN
      ldaps_dir6(k) = ((180./pi)*atan(ldaps_u6(k)/ldaps_v6(k)))+0.
      ELSE IF (ldaps_u6(k) .GT. 0. .AND. ldaps_v6(k).LT.0.) THEN
      ldaps_dir6(k) = ((180./pi)*atan(ldaps_u6(k)/ldaps_v6(k)))+360.
      ELSE
      ldaps_dir6(k) = 0.
      END IF
      ldaps_ws6(k) = SQRT(ldaps_u6(k)**2.0 + ldaps_v6(k)**2.0)

      IF (ldaps_v7(k).GT.0.) THEN
      ldaps_dir7(k) = ((180./pi)*atan(ldaps_u7(k)/ldaps_v7(k)))+180.
      ELSE IF (ldaps_u7(k) .LT. 0. .AND. ldaps_v7(k).LT.0.) THEN
      ldaps_dir7(k) = ((180./pi)*atan(ldaps_u7(k)/ldaps_v7(k)))+0.
      ELSE IF (ldaps_u7(k) .GT. 0. .AND. ldaps_v7(k).LT.0.) THEN
      ldaps_dir7(k) = ((180./pi)*atan(ldaps_u7(k)/ldaps_v7(k)))+360.
      ELSE
      ldaps_dir7(k) = 0.
      END IF
      ldaps_ws7(k) = SQRT(ldaps_u7(k)**2.0 + ldaps_v7(k)**2.0)

      IF (ldaps_v8(k).GT.0.) THEN
      ldaps_dir8(k) = ((180./pi)*atan(ldaps_u8(k)/ldaps_v8(k)))+180.
      ELSE IF (ldaps_u8(k) .LT. 0. .AND. ldaps_v8(k).LT.0.) THEN
      ldaps_dir8(k) = ((180./pi)*atan(ldaps_u8(k)/ldaps_v8(k)))+0.
      ELSE IF (ldaps_u8(k) .GT. 0. .AND. ldaps_v8(k).LT.0.) THEN
      ldaps_dir8(k) = ((180./pi)*atan(ldaps_u8(k)/ldaps_v8(k)))+360.
      ELSE
      ldaps_dir8(k) = 0.
      END IF
      ldaps_ws8(k) = SQRT(ldaps_u8(k)**2.0 + ldaps_v8(k)**2.0)

      IF (ldaps_v9(k).GT.0.) THEN
      ldaps_dir9(k) = ((180./pi)*atan(ldaps_u9(k)/ldaps_v9(k)))+180.
      ELSE IF (ldaps_u9(k) .LT. 0. .AND. ldaps_v9(k).LT.0.) THEN
      ldaps_dir9(k) = ((180./pi)*atan(ldaps_u9(k)/ldaps_v9(k)))+0.
      ELSE IF (ldaps_u9(k) .GT. 0. .AND. ldaps_v9(k).LT.0.) THEN
      ldaps_dir9(k) = ((180./pi)*atan(ldaps_u9(k)/ldaps_v9(k)))+360.
      ELSE
      ldaps_dir9(k) = 0.
      END IF
      ldaps_ws9(k) = SQRT(ldaps_u9(k)**2.0 + ldaps_v9(k)**2.0)

      IF (ldaps_v10(k).GT.0.) THEN
      ldaps_dir10(k) = ((180./pi)*atan(ldaps_u10(k)/ldaps_v10(k)))+180.
      ELSE IF (ldaps_u10(k) .LT. 0. .AND. ldaps_v10(k).LT.0.) THEN
      ldaps_dir10(k) = ((180./pi)*atan(ldaps_u10(k)/ldaps_v10(k)))+0.
      ELSE IF (ldaps_u10(k) .GT. 0. .AND. ldaps_v10(k).LT.0.) THEN
      ldaps_dir10(k) = ((180./pi)*atan(ldaps_u10(k)/ldaps_v10(k)))+360.
      ELSE
      ldaps_dir10(k) = 0.
      END IF
      ldaps_ws10(k) = SQRT(ldaps_u10(k)**2.0 + ldaps_v10(k)**2.0)

      IF (ldaps_v11(k).GT.0.) THEN
      ldaps_dir11(k) = ((180./pi)*atan(ldaps_u11(k)/ldaps_v11(k)))+180.
      ELSE IF (ldaps_u11(k) .LT. 0. .AND. ldaps_v11(k).LT.0.) THEN
      ldaps_dir11(k) = ((180./pi)*atan(ldaps_u11(k)/ldaps_v11(k)))+0.
      ELSE IF (ldaps_u11(k) .GT. 0. .AND. ldaps_v11(k).LT.0.) THEN
      ldaps_dir11(k) = ((180./pi)*atan(ldaps_u11(k)/ldaps_v11(k)))+360.
      ELSE
      ldaps_dir11(k) = 0.
      END IF
      ldaps_ws11(k) = SQRT(ldaps_u11(k)**2.0 + ldaps_v11(k)**2.0)

      IF (ldaps_v12(k).GT.0.) THEN
      ldaps_dir12(k) = ((180./pi)*atan(ldaps_u12(k)/ldaps_v12(k)))+180.
      ELSE IF (ldaps_u12(k) .LT. 0. .AND. ldaps_v12(k).LT.0.) THEN
      ldaps_dir12(k) = ((180./pi)*atan(ldaps_u12(k)/ldaps_v12(k)))+0.
      ELSE IF (ldaps_u12(k) .GT. 0. .AND. ldaps_v12(k).LT.0.) THEN
      ldaps_dir12(k) = ((180./pi)*atan(ldaps_u12(k)/ldaps_v12(k)))+360.
      ELSE
      ldaps_dir12(k) = 0.
      END IF
      ldaps_ws12(k) = SQRT(ldaps_u12(k)**2.0 + ldaps_v12(k)**2.0)

      IF (ldaps_v13(k).GT.0.) THEN
      ldaps_dir13(k) = ((180./pi)*atan(ldaps_u13(k)/ldaps_v13(k)))+180.
      ELSE IF (ldaps_u13(k) .LT. 0. .AND. ldaps_v13(k).LT.0.) THEN
      ldaps_dir13(k) = ((180./pi)*atan(ldaps_u13(k)/ldaps_v13(k)))+0.
      ELSE IF (ldaps_u13(k) .GT. 0. .AND. ldaps_v13(k).LT.0.) THEN
      ldaps_dir13(k) = ((180./pi)*atan(ldaps_u13(k)/ldaps_v13(k)))+360.
      ELSE
      ldaps_dir13(k) = 0.
      END IF
      ldaps_ws13(k) = SQRT(ldaps_u13(k)**2.0 + ldaps_v13(k)**2.0)

      IF (ldaps_v14(k).GT.0.) THEN
      ldaps_dir14(k) = ((180./pi)*atan(ldaps_u14(k)/ldaps_v14(k)))+180.
      ELSE IF (ldaps_u14(k) .LT. 0. .AND. ldaps_v14(k).LT.0.) THEN
      ldaps_dir14(k) = ((180./pi)*atan(ldaps_u14(k)/ldaps_v14(k)))+0.
      ELSE IF (ldaps_u14(k) .GT. 0. .AND. ldaps_v14(k).LT.0.) THEN
      ldaps_dir14(k) = ((180./pi)*atan(ldaps_u14(k)/ldaps_v14(k)))+360.
      ELSE
      ldaps_dir14(k) = 0.
      END IF
      ldaps_ws14(k) = SQRT(ldaps_u14(k)**2.0 + ldaps_v14(k)**2.0)

      IF (ldaps_v15(k).GT.0.) THEN
      ldaps_dir15(k) = ((180./pi)*atan(ldaps_u15(k)/ldaps_v15(k)))+180.
      ELSE IF (ldaps_u15(k) .LT. 0. .AND. ldaps_v15(k).LT.0.) THEN
      ldaps_dir15(k) = ((180./pi)*atan(ldaps_u15(k)/ldaps_v15(k)))+0.
      ELSE IF (ldaps_u15(k) .GT. 0. .AND. ldaps_v15(k).LT.0.) THEN
      ldaps_dir15(k) = ((180./pi)*atan(ldaps_u15(k)/ldaps_v15(k)))+360.
      ELSE
      ldaps_dir15(k) = 0.
      END IF
      ldaps_ws15(k) = SQRT(ldaps_u15(k)**2.0 + ldaps_v15(k)**2.0)

      IF (ldaps_v16(k).GT.0.) THEN
      ldaps_dir16(k) = ((180./pi)*atan(ldaps_u16(k)/ldaps_v16(k)))+180.
      ELSE IF (ldaps_u16(k) .LT. 0. .AND. ldaps_v16(k).LT.0.) THEN
      ldaps_dir16(k) = ((180./pi)*atan(ldaps_u16(k)/ldaps_v16(k)))+0.
      ELSE IF (ldaps_u16(k) .GT. 0. .AND. ldaps_v16(k).LT.0.) THEN
      ldaps_dir16(k) = ((180./pi)*atan(ldaps_u16(k)/ldaps_v16(k)))+360.
      ELSE
      ldaps_dir16(k) = 0.
      END IF
      ldaps_ws16(k) = SQRT(ldaps_u16(k)**2.0 + ldaps_v16(k)**2.0)

      END DO

!!!!!!!!
     WRITE(ofile, 777) './data/Output/e_SOFTCUBE_',sc_yy,sc_mm,sc_dd,sc_hh,'.dat'
     OPEN(99, file=TRIM(ofile), status='replace', action='write', &
          iostat=output_ios, iomsg=output_iomsg)
     IF (output_ios .NE. 0) THEN
       WRITE(*,'(A)') 'Error: unable to create output file.'
       WRITE(*,'(A)') TRIM(output_iomsg)
       WRITE(*,'(A)') 'Create the output/ directory and try again.'
       STOP 1
     END IF

     DO kk = 1, MIN(10, kmax2) !!!!!!!! Vertical Cycle Loop (floors 1-10) !!!!!!!!!! 

     domain_bg_u = [ldaps_u1(kk), ldaps_u2(kk), ldaps_u3(kk), ldaps_u4(kk), &
                    ldaps_u5(kk), ldaps_u6(kk), ldaps_u7(kk), ldaps_u8(kk), &
                    ldaps_u9(kk), ldaps_u10(kk), ldaps_u11(kk), ldaps_u12(kk), &
                    ldaps_u13(kk), ldaps_u14(kk), ldaps_u15(kk), ldaps_u16(kk)]
     domain_bg_v = [ldaps_v1(kk), ldaps_v2(kk), ldaps_v3(kk), ldaps_v4(kk), &
                    ldaps_v5(kk), ldaps_v6(kk), ldaps_v7(kk), ldaps_v8(kk), &
                    ldaps_v9(kk), ldaps_v10(kk), ldaps_v11(kk), ldaps_v12(kk), &
                    ldaps_v13(kk), ldaps_v14(kk), ldaps_v15(kk), ldaps_v16(kk)]
     domain_bg_t = [ldaps_t1(kk), ldaps_t2(kk), ldaps_t3(kk), ldaps_t4(kk), &
                    ldaps_t5(kk), ldaps_t6(kk), ldaps_t7(kk), ldaps_t8(kk), &
                    ldaps_t9(kk), ldaps_t10(kk), ldaps_t11(kk), ldaps_t12(kk), &
                    ldaps_t13(kk), ldaps_t14(kk), ldaps_t15(kk), ldaps_t16(kk)]
     inflow_at_level = [inflow_ws_01(kk), inflow_ws_02(kk), &
                        inflow_ws_03(kk), inflow_ws_04(kk), &
                        inflow_ws_05(kk), inflow_ws_06(kk), &
                        inflow_ws_07(kk), inflow_ws_08(kk)]

     DO wind_domain = 1, 16
       domain_ws(wind_domain) = SQRT(domain_bg_u(wind_domain)**2.0 + &
                                      domain_bg_v(wind_domain)**2.0)
       IF (domain_ws(wind_domain) < inflow_at_level(1)) THEN
         cfd_ws_case(wind_domain,:) = [1,1]
         ws_lower_value(wind_domain) = inflow_at_level(1)
         ws_upper_value(wind_domain) = inflow_at_level(1)
       ELSE IF (domain_ws(wind_domain) > inflow_at_level(8)) THEN
         cfd_ws_case(wind_domain,:) = [8,8]
         ws_lower_value(wind_domain) = inflow_at_level(8)
         ws_upper_value(wind_domain) = inflow_at_level(8)
       ELSE
         DO wind_case = 1, 7
           IF (domain_ws(wind_domain) >= inflow_at_level(wind_case) .AND. &
               domain_ws(wind_domain) <= inflow_at_level(wind_case+1)) THEN
             cfd_ws_case(wind_domain,:) = [wind_case,wind_case+1]
             ws_lower_value(wind_domain) = inflow_at_level(wind_case)
             ws_upper_value(wind_domain) = inflow_at_level(wind_case+1)
             EXIT
           END IF
         END DO
       END IF
       IF (domain_bg_v(wind_domain) > 0.0) THEN
         domain_met_dir(wind_domain) = (180.0/pi)* &
              ATAN(domain_bg_u(wind_domain)/domain_bg_v(wind_domain))+180.0
       ELSE IF (domain_bg_u(wind_domain) < 0.0 .AND. domain_bg_v(wind_domain) < 0.0) THEN
         domain_met_dir(wind_domain) = (180.0/pi)* &
              ATAN(domain_bg_u(wind_domain)/domain_bg_v(wind_domain))
       ELSE IF (domain_bg_u(wind_domain) > 0.0 .AND. domain_bg_v(wind_domain) < 0.0) THEN
         domain_met_dir(wind_domain) = (180.0/pi)* &
              ATAN(domain_bg_u(wind_domain)/domain_bg_v(wind_domain))+360.0
       ELSE
         domain_met_dir(wind_domain) = 0.0
       END IF
       IF (domain_met_dir(wind_domain) <= 270.0) &
         domain_cfd_dir(wind_domain)=270.0-domain_met_dir(wind_domain)
       IF (domain_met_dir(wind_domain) >= 270.0) &
         domain_cfd_dir(wind_domain)=360.0-(domain_met_dir(wind_domain)-270.0)
       domain_alpha(wind_domain)=0.0
       cfd_angle_case(wind_domain,:)=[1,2]
       DO wind_case=1,32
         IF (domain_cfd_dir(wind_domain) >= REAL(wind_case-1)*11.25) THEN
           domain_alpha(wind_domain)=REAL(wind_case-1)*11.25
           cfd_angle_case(wind_domain,:)=[wind_case,wind_case+1]
         END IF
       END DO
       ws_delta_lower(wind_domain)=domain_ws(wind_domain)-ws_lower_value(wind_domain)
       ws_delta_upper(wind_domain)=ws_upper_value(wind_domain)-domain_ws(wind_domain)
       wd_weight_lower(wind_domain)= &
            (11.25-(domain_cfd_dir(wind_domain)-domain_alpha(wind_domain)))/11.25
       wd_weight_upper(wind_domain)= &
            (domain_cfd_dir(wind_domain)-domain_alpha(wind_domain))/11.25
     END DO

!!!!!!!!!!!!!!!!!!!! Unified D01-D16 DB read/interpolation !!!!!!!!!!!!!!!!!!!!
!c Corner order: surface class (low/high), wind speed (low/high),
!c and direction (low/high).  This is the same eight-corner interpolation
!c used by each of the former D01-D16 blocks.
      surface_name = ['Roof ','Wall ','Road ','Green','Soil ','Water']
      uu_domain = 0.0
      vv_domain = 0.0
      ww_domain = 0.0
      ftemp_domain = 0.0

      DO db_domain = 1, 16
        DO db_surface = 1, 6
          DO db_corner = 1, 8
            class_slot = 1 + (db_corner-1)/4
            speed_slot = 1 + MOD((db_corner-1)/2,2)
            angle_slot = 1 + MOD(db_corner-1,2)

            WRITE(db_file,901) db_domain, TRIM(surface_name(db_surface)), &
                 surface_class_case(db_domain,db_surface,class_slot), &
                 cfd_ws_case(db_domain,speed_slot), kk, &
                 cfd_angle_case(db_domain,angle_slot)
  901       FORMAT('./data/DB/Layer/D',I2.2,'/',A,'/Class_',I1, &
                   '/Log_',I2.2,'/k=',I3.3,'_moment_',I2.2,'.dat')

            db_unit = 70 + db_corner
            OPEN(db_unit,file=TRIM(db_file),status='old',action='read')
            DO j = 1, jmax2
              DO i = 1, imax2
                SELECT CASE (db_corner)
                CASE (1)
                  READ(db_unit,*) uu_db1(i,j),vv_db1(i,j),ww_db1(i,j), &
                                  dt_db1(i,j),rt_db1(i,j)
                CASE (2)
                  READ(db_unit,*) uu_db2(i,j),vv_db2(i,j),ww_db2(i,j), &
                                  dt_db2(i,j),rt_db2(i,j)
                CASE (3)
                  READ(db_unit,*) uu_db3(i,j),vv_db3(i,j),ww_db3(i,j), &
                                  dt_db3(i,j),rt_db3(i,j)
                CASE (4)
                  READ(db_unit,*) uu_db4(i,j),vv_db4(i,j),ww_db4(i,j), &
                                  dt_db4(i,j),rt_db4(i,j)
                CASE (5)
                  READ(db_unit,*) uu_db5(i,j),vv_db5(i,j),ww_db5(i,j), &
                                  dt_db5(i,j),rt_db5(i,j)
                CASE (6)
                  READ(db_unit,*) uu_db6(i,j),vv_db6(i,j),ww_db6(i,j), &
                                  dt_db6(i,j),rt_db6(i,j)
                CASE (7)
                  READ(db_unit,*) uu_db7(i,j),vv_db7(i,j),ww_db7(i,j), &
                                  dt_db7(i,j),rt_db7(i,j)
                CASE (8)
                  READ(db_unit,*) uu_db8(i,j),vv_db8(i,j),ww_db8(i,j), &
                                  dt_db8(i,j),rt_db8(i,j)
                END SELECT
              END DO
            END DO
            CLOSE(db_unit)
          END DO

          IF (surface_class_case(db_domain,db_surface,1) == &
              surface_class_case(db_domain,db_surface,2)) THEN
            surface_weight = [1.0,0.0]
          ELSE
            weight_sum = surface_delta_lower(db_domain,db_surface) + &
                         surface_delta_upper(db_domain,db_surface)
            surface_weight(1) = &
                 surface_delta_upper(db_domain,db_surface)/weight_sum
            surface_weight(2) = &
                 surface_delta_lower(db_domain,db_surface)/weight_sum
          END IF

          velocity_scale = 1.0
          IF (cfd_ws_case(db_domain,1) == cfd_ws_case(db_domain,2)) THEN
            speed_weight = [1.0,0.0]
            IF (ws_lower_value(db_domain) > 0.0) &
              velocity_scale = domain_ws(db_domain)/ws_lower_value(db_domain)
          ELSE
            weight_sum = ws_delta_lower(db_domain) + &
                         ws_delta_upper(db_domain)
            speed_weight(1) = ws_delta_upper(db_domain)/weight_sum
            speed_weight(2) = ws_delta_lower(db_domain)/weight_sum
          END IF
          angle_weight = [wd_weight_lower(db_domain), &
                          wd_weight_upper(db_domain)]

          DO db_corner = 1, 8
            class_slot = 1 + (db_corner-1)/4
            speed_slot = 1 + MOD((db_corner-1)/2,2)
            angle_slot = 1 + MOD(db_corner-1,2)
            corner_weight(db_corner) = surface_weight(class_slot) * &
                 speed_weight(speed_slot) * angle_weight(angle_slot)
          END DO

          u_interp = corner_weight(1)*uu_db1 + corner_weight(2)*uu_db2 + &
                     corner_weight(3)*uu_db3 + corner_weight(4)*uu_db4 + &
                     corner_weight(5)*uu_db5 + corner_weight(6)*uu_db6 + &
                     corner_weight(7)*uu_db7 + corner_weight(8)*uu_db8
          v_interp = corner_weight(1)*vv_db1 + corner_weight(2)*vv_db2 + &
                     corner_weight(3)*vv_db3 + corner_weight(4)*vv_db4 + &
                     corner_weight(5)*vv_db5 + corner_weight(6)*vv_db6 + &
                     corner_weight(7)*vv_db7 + corner_weight(8)*vv_db8
          w_interp = corner_weight(1)*ww_db1 + corner_weight(2)*ww_db2 + &
                     corner_weight(3)*ww_db3 + corner_weight(4)*ww_db4 + &
                     corner_weight(5)*ww_db5 + corner_weight(6)*ww_db6 + &
                     corner_weight(7)*ww_db7 + corner_weight(8)*ww_db8
          dtemp_interp = corner_weight(1)*dt_db1 + &
                         corner_weight(2)*dt_db2 + &
                         corner_weight(3)*dt_db3 + &
                         corner_weight(4)*dt_db4 + &
                         corner_weight(5)*dt_db5 + &
                         corner_weight(6)*dt_db6 + &
                         corner_weight(7)*dt_db7 + &
                         corner_weight(8)*dt_db8
          rtemp_interp = corner_weight(1)*rt_db1 + &
                         corner_weight(2)*rt_db2 + &
                         corner_weight(3)*rt_db3 + &
                         corner_weight(4)*rt_db4 + &
                         corner_weight(5)*rt_db5 + &
                         corner_weight(6)*rt_db6 + &
                         corner_weight(7)*rt_db7 + &
                         corner_weight(8)*rt_db8

          uu_domain(:,:,db_domain) = &
               uu_domain(:,:,db_domain) + velocity_scale*u_interp/6.0
          vv_domain(:,:,db_domain) = &
               vv_domain(:,:,db_domain) + velocity_scale*v_interp/6.0
          ww_domain(:,:,db_domain) = &
               ww_domain(:,:,db_domain) + velocity_scale*w_interp/6.0
          ftemp_domain(:,:,db_domain) = &
               ftemp_domain(:,:,db_domain) + rtemp_interp
        END DO

        ftemp_domain(:,:,db_domain) = domain_bg_t(db_domain) * &
             (ftemp_domain(:,:,db_domain)-5.0)
      END DO
      uu1 = uu_domain(:,:,1)
      vv1 = vv_domain(:,:,1)
      ww1 = ww_domain(:,:,1)
      ftemp1 = ftemp_domain(:,:,1)
      uu2 = uu_domain(:,:,2)
      vv2 = vv_domain(:,:,2)
      ww2 = ww_domain(:,:,2)
      ftemp2 = ftemp_domain(:,:,2)
      uu3 = uu_domain(:,:,3)
      vv3 = vv_domain(:,:,3)
      ww3 = ww_domain(:,:,3)
      ftemp3 = ftemp_domain(:,:,3)
      uu4 = uu_domain(:,:,4)
      vv4 = vv_domain(:,:,4)
      ww4 = ww_domain(:,:,4)
      ftemp4 = ftemp_domain(:,:,4)
      uu5 = uu_domain(:,:,5)
      vv5 = vv_domain(:,:,5)
      ww5 = ww_domain(:,:,5)
      ftemp5 = ftemp_domain(:,:,5)
      uu6 = uu_domain(:,:,6)
      vv6 = vv_domain(:,:,6)
      ww6 = ww_domain(:,:,6)
      ftemp6 = ftemp_domain(:,:,6)
      uu7 = uu_domain(:,:,7)
      vv7 = vv_domain(:,:,7)
      ww7 = ww_domain(:,:,7)
      ftemp7 = ftemp_domain(:,:,7)
      uu8 = uu_domain(:,:,8)
      vv8 = vv_domain(:,:,8)
      ww8 = ww_domain(:,:,8)
      ftemp8 = ftemp_domain(:,:,8)
      uu9 = uu_domain(:,:,9)
      vv9 = vv_domain(:,:,9)
      ww9 = ww_domain(:,:,9)
      ftemp9 = ftemp_domain(:,:,9)
      uu10 = uu_domain(:,:,10)
      vv10 = vv_domain(:,:,10)
      ww10 = ww_domain(:,:,10)
      ftemp10 = ftemp_domain(:,:,10)
      uu11 = uu_domain(:,:,11)
      vv11 = vv_domain(:,:,11)
      ww11 = ww_domain(:,:,11)
      ftemp11 = ftemp_domain(:,:,11)
      uu12 = uu_domain(:,:,12)
      vv12 = vv_domain(:,:,12)
      ww12 = ww_domain(:,:,12)
      ftemp12 = ftemp_domain(:,:,12)
      uu13 = uu_domain(:,:,13)
      vv13 = vv_domain(:,:,13)
      ww13 = ww_domain(:,:,13)
      ftemp13 = ftemp_domain(:,:,13)
      uu14 = uu_domain(:,:,14)
      vv14 = vv_domain(:,:,14)
      ww14 = ww_domain(:,:,14)
      ftemp14 = ftemp_domain(:,:,14)
      uu15 = uu_domain(:,:,15)
      vv15 = vv_domain(:,:,15)
      ww15 = ww_domain(:,:,15)
      ftemp15 = ftemp_domain(:,:,15)
      uu16 = uu_domain(:,:,16)
      vv16 = vv_domain(:,:,16)
      ww16 = ww_domain(:,:,16)
      ftemp16 = ftemp_domain(:,:,16)

      DO j=1, jmax2
      DO i=1, imax2
!c Domain surface superposition is already included in domain_result_* above.
!c Apply the domain- and layer-specific ML emulator corrections (Methods 4-6).
      uu1(i,j) = emulator_mr(kk,1) * uu1(i,j)
      vv1(i,j) = emulator_mr(kk,1) * vv1(i,j)
      ww1(i,j) = emulator_mr(kk,1) * ww1(i,j)
      ftemp1(i,j) = emulator_ct(kk,1) * ftemp1(i,j)
      uu2(i,j) = emulator_mr(kk,2) * uu2(i,j)
      vv2(i,j) = emulator_mr(kk,2) * vv2(i,j)
      ww2(i,j) = emulator_mr(kk,2) * ww2(i,j)
      ftemp2(i,j) = emulator_ct(kk,2) * ftemp2(i,j)
      uu3(i,j) = emulator_mr(kk,3) * uu3(i,j)
      vv3(i,j) = emulator_mr(kk,3) * vv3(i,j)
      ww3(i,j) = emulator_mr(kk,3) * ww3(i,j)
      ftemp3(i,j) = emulator_ct(kk,3) * ftemp3(i,j)
      uu4(i,j) = emulator_mr(kk,4) * uu4(i,j)
      vv4(i,j) = emulator_mr(kk,4) * vv4(i,j)
      ww4(i,j) = emulator_mr(kk,4) * ww4(i,j)
      ftemp4(i,j) = emulator_ct(kk,4) * ftemp4(i,j)
      uu5(i,j) = emulator_mr(kk,5) * uu5(i,j)
      vv5(i,j) = emulator_mr(kk,5) * vv5(i,j)
      ww5(i,j) = emulator_mr(kk,5) * ww5(i,j)
      ftemp5(i,j) = emulator_ct(kk,5) * ftemp5(i,j)
      uu6(i,j) = emulator_mr(kk,6) * uu6(i,j)
      vv6(i,j) = emulator_mr(kk,6) * vv6(i,j)
      ww6(i,j) = emulator_mr(kk,6) * ww6(i,j)
      ftemp6(i,j) = emulator_ct(kk,6) * ftemp6(i,j)
      uu7(i,j) = emulator_mr(kk,7) * uu7(i,j)
      vv7(i,j) = emulator_mr(kk,7) * vv7(i,j)
      ww7(i,j) = emulator_mr(kk,7) * ww7(i,j)
      ftemp7(i,j) = emulator_ct(kk,7) * ftemp7(i,j)
      uu8(i,j) = emulator_mr(kk,8) * uu8(i,j)
      vv8(i,j) = emulator_mr(kk,8) * vv8(i,j)
      ww8(i,j) = emulator_mr(kk,8) * ww8(i,j)
      ftemp8(i,j) = emulator_ct(kk,8) * ftemp8(i,j)
      uu9(i,j) = emulator_mr(kk,9) * uu9(i,j)
      vv9(i,j) = emulator_mr(kk,9) * vv9(i,j)
      ww9(i,j) = emulator_mr(kk,9) * ww9(i,j)
      ftemp9(i,j) = emulator_ct(kk,9) * ftemp9(i,j)
      uu10(i,j) = emulator_mr(kk,10) * uu10(i,j)
      vv10(i,j) = emulator_mr(kk,10) * vv10(i,j)
      ww10(i,j) = emulator_mr(kk,10) * ww10(i,j)
      ftemp10(i,j) = emulator_ct(kk,10) * ftemp10(i,j)
      uu11(i,j) = emulator_mr(kk,11) * uu11(i,j)
      vv11(i,j) = emulator_mr(kk,11) * vv11(i,j)
      ww11(i,j) = emulator_mr(kk,11) * ww11(i,j)
      ftemp11(i,j) = emulator_ct(kk,11) * ftemp11(i,j)
      uu12(i,j) = emulator_mr(kk,12) * uu12(i,j)
      vv12(i,j) = emulator_mr(kk,12) * vv12(i,j)
      ww12(i,j) = emulator_mr(kk,12) * ww12(i,j)
      ftemp12(i,j) = emulator_ct(kk,12) * ftemp12(i,j)
      uu13(i,j) = emulator_mr(kk,13) * uu13(i,j)
      vv13(i,j) = emulator_mr(kk,13) * vv13(i,j)
      ww13(i,j) = emulator_mr(kk,13) * ww13(i,j)
      ftemp13(i,j) = emulator_ct(kk,13) * ftemp13(i,j)
      uu14(i,j) = emulator_mr(kk,14) * uu14(i,j)
      vv14(i,j) = emulator_mr(kk,14) * vv14(i,j)
      ww14(i,j) = emulator_mr(kk,14) * ww14(i,j)
      ftemp14(i,j) = emulator_ct(kk,14) * ftemp14(i,j)
      uu15(i,j) = emulator_mr(kk,15) * uu15(i,j)
      vv15(i,j) = emulator_mr(kk,15) * vv15(i,j)
      ww15(i,j) = emulator_mr(kk,15) * ww15(i,j)
      ftemp15(i,j) = emulator_ct(kk,15) * ftemp15(i,j)
      uu16(i,j) = emulator_mr(kk,16) * uu16(i,j)
      vv16(i,j) = emulator_mr(kk,16) * vv16(i,j)
      ww16(i,j) = emulator_mr(kk,16) * ww16(i,j)
      ftemp16(i,j) = emulator_ct(kk,16) * ftemp16(i,j)

      ENDDO
      ENDDO


!!!!!!!! Integral D01 ~ D16 (148/10/138 scheme; final 602×602) !!!!!!!!!

!========================
! 하단 행 (D13 ~ D16)  j = 1..148
!========================
! D13
DO j=1,148
  DO i=1,148
    fuu(i,j) = uu13(i,j)
    fvv(i,j) = vv13(i,j)
    fww(i,j) = ww13(i,j)
    ftemp(i,j) = ftemp13(i,j)
  ENDDO
ENDDO

!! X1 Buffer: D13 & D14, i = 149..158
DO j=1,148
  DO i=149,158
    fuu(i,j) = ((158-i)*uu13(i,j)     + (i-148)*uu14(i-148,j))/10.
    fvv(i,j) = ((158-i)*vv13(i,j)     + (i-148)*vv14(i-148,j))/10.
    fww(i,j) = ((158-i)*ww13(i,j)     + (i-148)*ww14(i-148,j))/10.
    ftemp(i,j) = ((158-i)*ftemp13(i,j)     + (i-148)*ftemp14(i-148,j))/10.
  ENDDO
ENDDO

! D14 (코어): i = 159..296
DO j=1,148
  DO i=159,296
    fuu(i,j) = uu14(i-148,j)
    fvv(i,j) = vv14(i-148,j)
    fww(i,j) = ww14(i-148,j)
    ftemp(i,j) = ftemp14(i-148,j)
  ENDDO
ENDDO

!! X2 Buffer: D14 & D15, i = 297..306
DO j=1,148
  DO i=297,306
    fuu(i,j) = ((306-i)*uu14(i-148,j) + (i-296)*uu15(i-296,j))/10.
    fvv(i,j) = ((306-i)*vv14(i-148,j) + (i-296)*vv15(i-296,j))/10.
    fww(i,j) = ((306-i)*ww14(i-148,j) + (i-296)*ww15(i-296,j))/10.
    ftemp(i,j) = ((306-i)*ftemp14(i-148,j) + (i-296)*ftemp15(i-296,j))/10.
  ENDDO
ENDDO

! D15 (코어): i = 307..444
DO j=1,148
  DO i=307,444
    fuu(i,j) = uu15(i-296,j)
    fvv(i,j) = vv15(i-296,j)
    fww(i,j) = ww15(i-296,j)
    ftemp(i,j) = ftemp15(i-296,j)
  ENDDO
ENDDO

!! X3 Buffer: D15 & D16, i = 445..454
DO j=1,148
  DO i=445,454
    fuu(i,j) = ((454-i)*uu15(i-296,j) + (i-444)*uu16(i-444,j))/10.
    fvv(i,j) = ((454-i)*vv15(i-296,j) + (i-444)*vv16(i-444,j))/10.
    fww(i,j) = ((454-i)*ww15(i-296,j) + (i-444)*ww16(i-444,j))/10.
    ftemp(i,j) = ((454-i)*ftemp15(i-296,j) + (i-444)*ftemp16(i-444,j))/10.
  ENDDO
ENDDO

! D16 (코어): i = 455..602
DO j=1,148
  DO i=455,602
    fuu(i,j) = uu16(i-444,j)
    fvv(i,j) = vv16(i-444,j)
    fww(i,j) = ww16(i-444,j)
    ftemp(i,j) = ftemp16(i-444,j)
  ENDDO
ENDDO


!========================================
! Y1 버퍼 (D13~D16 ↔ D09~D12), j = 149..158
!========================================

!! Y1 First Buffer: D13 & D09, i = 1..148
DO j=149,158
  DO i=1,148
    fuu(i,j) = ((158-j)*uu13(i,j)     + (j-148)*uu9(i, j-148))/10.
    fvv(i,j) = ((158-j)*vv13(i,j)     + (j-148)*vv9(i, j-148))/10.
    fww(i,j) = ((158-j)*ww13(i,j)     + (j-148)*ww9(i, j-148))/10.
    ftemp(i,j) = ((158-j)*ftemp13(i,j)     + (j-148)*ftemp9(i, j-148))/10.
  ENDDO
ENDDO

!! Y1 Square First: (D13,D14,D09,D10), i = 149..158
DO j=149,158
  wyS=(158-j)/10.
  wyN=(j-148)/10.
  DO i=149,158
    wxW=(158-i)/10.
    wxE=(i-148)/10.
    fuu(i,j)= wxW*wyS*uu13(i,      j    ) + wxE*wyS*uu14(i-148, j    ) + wxW*wyN*uu9 (i,      j-148) + wxE*wyN*uu10(i-148, j-148)
    fvv(i,j)= wxW*wyS*vv13(i,      j    ) + wxE*wyS*vv14(i-148, j    ) + wxW*wyN*vv9 (i,      j-148) + wxE*wyN*vv10(i-148, j-148)
    fww(i,j)= wxW*wyS*ww13(i,      j    ) + wxE*wyS*ww14(i-148, j    ) + wxW*wyN*ww9 (i,      j-148) + wxE*wyN*ww10(i-148, j-148)
    ftemp(i,j)= wxW*wyS*ftemp13(i,      j    ) + wxE*wyS*ftemp14(i-148, j    ) + wxW*wyN*ftemp9 (i,      j-148) + wxE*wyN*ftemp10(i-148, j-148)
  ENDDO
ENDDO

!! Y1 Second Buffer: D14 & D10, i = 159..296
DO j=149,158
  DO i=159,296
    fuu(i,j) = ((158-j)*uu14(i-148,j) + (j-148)*uu10(i-148, j-148))/10.
    fvv(i,j) = ((158-j)*vv14(i-148,j) + (j-148)*vv10(i-148, j-148))/10.
    fww(i,j) = ((158-j)*ww14(i-148,j) + (j-148)*ww10(i-148, j-148))/10.
    ftemp(i,j) = ((158-j)*ftemp14(i-148,j) + (j-148)*ftemp10(i-148, j-148))/10.
  ENDDO
ENDDO

!! Y1 Square Second: (D14,D15,D10,D11), i = 297..306
DO j=149,158
  wyS=(158-j)/10.
  wyN=(j-148)/10.
  DO i=297,306
    wxW=(306-i)/10.
    wxE=(i-296)/10.
    fuu(i,j)= wxW*wyS*uu14(i-148, j    ) + wxE*wyS*uu15(i-296, j    ) + wxW*wyN*uu10(i-148, j-148) + wxE*wyN*uu11(i-296, j-148)
    fvv(i,j)= wxW*wyS*vv14(i-148, j    ) + wxE*wyS*vv15(i-296, j    ) + wxW*wyN*vv10(i-148, j-148) + wxE*wyN*vv11(i-296, j-148)
    fww(i,j)= wxW*wyS*ww14(i-148, j    ) + wxE*wyS*ww15(i-296, j    ) + wxW*wyN*ww10(i-148, j-148) + wxE*wyN*ww11(i-296, j-148)
    ftemp(i,j)= wxW*wyS*ftemp14(i-148, j    ) + wxE*wyS*ftemp15(i-296, j    ) + wxW*wyN*ftemp10(i-148, j-148) + wxE*wyN*ftemp11(i-296, j-148)
  ENDDO
ENDDO

!! Y1 Third Buffer: D15 & D11, i = 307..444
DO j=149,158
  DO i=307,444
    fuu(i,j) = ((158-j)*uu15(i-296,j) + (j-148)*uu11(i-296, j-148))/10.
    fvv(i,j) = ((158-j)*vv15(i-296,j) + (j-148)*vv11(i-296, j-148))/10.
    fww(i,j) = ((158-j)*ww15(i-296,j) + (j-148)*ww11(i-296, j-148))/10.
    ftemp(i,j) = ((158-j)*ftemp15(i-296,j) + (j-148)*ftemp11(i-296, j-148))/10.
  ENDDO
ENDDO

!! Y1 Square Third: (D15,D16,D11,D12), i = 445..454
DO j=149,158
  wyS=(158-j)/10.
  wyN=(j-148)/10.
  DO i=445,454
    wxW=(454-i)/10.
    wxE=(i-444)/10.
    fuu(i,j)= wxW*wyS*uu15(i-296, j    ) + wxE*wyS*uu16(i-444, j    ) + wxW*wyN*uu11(i-296, j-148) + wxE*wyN*uu12(i-444, j-148)
    fvv(i,j)= wxW*wyS*vv15(i-296, j    ) + wxE*wyS*vv16(i-444, j    ) + wxW*wyN*vv11(i-296, j-148) + wxE*wyN*vv12(i-444, j-148)
    fww(i,j)= wxW*wyS*ww15(i-296, j    ) + wxE*wyS*ww16(i-444, j    ) + wxW*wyN*ww11(i-296, j-148) + wxE*wyN*ww12(i-444, j-148)
    ftemp(i,j)= wxW*wyS*ftemp15(i-296, j    ) + wxE*wyS*ftemp16(i-444, j    ) + wxW*wyN*ftemp11(i-296, j-148) + wxE*wyN*ftemp12(i-444, j-148)
  ENDDO
ENDDO

!! Y1 Fourth Buffer: D16 & D12, i = 455..602
DO j=149,158
  DO i=455,602
    fuu(i,j) = ((158-j)*uu16(i-444,j) + (j-148)*uu12(i-444, j-148))/10.
    fvv(i,j) = ((158-j)*vv16(i-444,j) + (j-148)*vv12(i-444, j-148))/10.
    fww(i,j) = ((158-j)*ww16(i-444,j) + (j-148)*ww12(i-444, j-148))/10.
    ftemp(i,j) = ((158-j)*ftemp16(i-444,j) + (j-148)*ftemp12(i-444, j-148))/10.
  ENDDO
ENDDO


!========================
! 중하단 행 (D09 ~ D12)  j = 159..296
!========================

! Copy 9
DO j=159,296
  DO i=1,148
    fuu(i,j) = uu9(i, j-148)
    fvv(i,j) = vv9(i, j-148)
    fww(i,j) = ww9(i, j-148)
    ftemp(i,j) = ftemp9(i, j-148)
  ENDDO
ENDDO

!! X1 Buffer (9-10): i = 149..158
DO j=159,296
  DO i=149,158
    fuu(i,j) = ((158-i)*uu9(i, j-148)   + (i-148)*uu10(i-148, j-148))/10.
    fvv(i,j) = ((158-i)*vv9(i, j-148)   + (i-148)*vv10(i-148, j-148))/10.
    fww(i,j) = ((158-i)*ww9(i, j-148)   + (i-148)*ww10(i-148, j-148))/10.
    ftemp(i,j) = ((158-i)*ftemp9(i, j-148)   + (i-148)*ftemp10(i-148, j-148))/10.
  ENDDO
ENDDO

! Copy 10: i = 159..296
DO j=159,296
  DO i=159,296
    fuu(i,j) = uu10(i-148, j-148)
    fvv(i,j) = vv10(i-148, j-148)
    fww(i,j) = ww10(i-148, j-148)
    ftemp(i,j) = ftemp10(i-148, j-148)
  ENDDO
ENDDO

!! X2 Buffer (10-11): i = 297..306
DO j=159,296
  DO i=297,306
    fuu(i,j) = ((306-i)*uu10(i-148, j-148) + (i-296)*uu11(i-296, j-148))/10.
    fvv(i,j) = ((306-i)*vv10(i-148, j-148) + (i-296)*vv11(i-296, j-148))/10.
    fww(i,j) = ((306-i)*ww10(i-148, j-148) + (i-296)*ww11(i-296, j-148))/10.
    ftemp(i,j) = ((306-i)*ftemp10(i-148, j-148) + (i-296)*ftemp11(i-296, j-148))/10.
  ENDDO
ENDDO

! Copy 11: i = 307..444
DO j=159,296
  DO i=307,444
    fuu(i,j) = uu11(i-296, j-148)
    fvv(i,j) = vv11(i-296, j-148)
    fww(i,j) = ww11(i-296, j-148)
    ftemp(i,j) = ftemp11(i-296, j-148)
  ENDDO
ENDDO

!! X3 Buffer (11-12): i = 445..454
DO j=159,296
  DO i=445,454
    fuu(i,j) = ((454-i)*uu11(i-296, j-148) + (i-444)*uu12(i-444, j-148))/10.
    fvv(i,j) = ((454-i)*vv11(i-296, j-148) + (i-444)*vv12(i-444, j-148))/10.
    fww(i,j) = ((454-i)*ww11(i-296, j-148) + (i-444)*ww12(i-444, j-148))/10.
    ftemp(i,j) = ((454-i)*ftemp11(i-296, j-148) + (i-444)*ftemp12(i-444, j-148))/10.
  ENDDO
ENDDO

! Copy 12: i = 455..602
DO j=159,296
  DO i=455,602
    fuu(i,j) = uu12(i-444, j-148)
    fvv(i,j) = vv12(i-444, j-148)
    fww(i,j) = ww12(i-444, j-148)
    ftemp(i,j) = ftemp12(i-444, j-148)
  ENDDO
ENDDO


!========================================
! Y2 버퍼 (D09~D12 ↔ D05~D08), j = 297..306
!========================================

!! Y2 First Buffer (9-5), i = 1..148
DO j=297,306
  DO i=1,148
    fuu(i,j) = ((306-j)*uu9(i, j-148)   + (j-296)*uu5(i, j-296))/10.
    fvv(i,j) = ((306-j)*vv9(i, j-148)   + (j-296)*vv5(i, j-296))/10.
    fww(i,j) = ((306-j)*ww9(i, j-148)   + (j-296)*ww5(i, j-296))/10.
    ftemp(i,j) = ((306-j)*ftemp9(i, j-148)   + (j-296)*ftemp5(i, j-296))/10.
  ENDDO
ENDDO

!! Y2 Square First (9,10,5,6), i = 149..158
DO j=297,306
  wyS=(306-j)/10.
  wyN=(j-296)/10.
  DO i=149,158
    wxW=(158-i)/10.
    wxE=(i-148)/10.
    fuu(i,j)= wxW*wyS*uu9 (i,      j-148) + wxE*wyS*uu10(i-148, j-148) + wxW*wyN*uu5 (i,      j-296) + wxE*wyN*uu6 (i-148, j-296)
    fvv(i,j)= wxW*wyS*vv9 (i,      j-148) + wxE*wyS*vv10(i-148, j-148) + wxW*wyN*vv5 (i,      j-296) + wxE*wyN*vv6 (i-148, j-296)
    fww(i,j)= wxW*wyS*ww9 (i,      j-148) + wxE*wyS*ww10(i-148, j-148) + wxW*wyN*ww5 (i,      j-296) + wxE*wyN*ww6 (i-148, j-296)
    ftemp(i,j)= wxW*wyS*ftemp9 (i,      j-148) + wxE*wyS*ftemp10(i-148, j-148) + wxW*wyN*ftemp5 (i,      j-296) + wxE*wyN*ftemp6 (i-148, j-296)
  ENDDO
ENDDO

!! Y2 Second Buffer (10-6), i = 159..296
DO j=297,306
  DO i=159,296
    fuu(i,j) = ((306-j)*uu10(i-148, j-148) + (j-296)*uu6(i-148, j-296))/10.
    fvv(i,j) = ((306-j)*vv10(i-148, j-148) + (j-296)*vv6(i-148, j-296))/10.
    fww(i,j) = ((306-j)*ww10(i-148, j-148) + (j-296)*ww6(i-148, j-296))/10.
    ftemp(i,j) = ((306-j)*ftemp10(i-148, j-148) + (j-296)*ftemp6(i-148, j-296))/10.
  ENDDO
ENDDO

!! Y2 Square Second (10,11,6,7), i = 297..306
DO j=297,306
  wyS=(306-j)/10.
  wyN=(j-296)/10.
  DO i=297,306
    wxW=(306-i)/10.
    wxE=(i-296)/10.
    fuu(i,j)= wxW*wyS*uu10(i-148, j-148) + wxE*wyS*uu11(i-296, j-148) + wxW*wyN*uu6 (i-148, j-296) + wxE*wyN*uu7 (i-296, j-296)
    fvv(i,j)= wxW*wyS*vv10(i-148, j-148) + wxE*wyS*vv11(i-296, j-148) + wxW*wyN*vv6 (i-148, j-296) + wxE*wyN*vv7 (i-296, j-296)
    fww(i,j)= wxW*wyS*ww10(i-148, j-148) + wxE*wyS*ww11(i-296, j-148) + wxW*wyN*ww6 (i-148, j-296) + wxE*wyN*ww7 (i-296, j-296)
    ftemp(i,j)= wxW*wyS*ftemp10(i-148, j-148) + wxE*wyS*ftemp11(i-296, j-148) + wxW*wyN*ftemp6 (i-148, j-296) + wxE*wyN*ftemp7 (i-296, j-296)
  ENDDO
ENDDO

!! Y2 Third Buffer (11-7), i = 307..444
DO j=297,306
  DO i=307,444
    fuu(i,j) = ((306-j)*uu11(i-296, j-148) + (j-296)*uu7(i-296, j-296))/10.
    fvv(i,j) = ((306-j)*vv11(i-296, j-148) + (j-296)*vv7(i-296, j-296))/10.
    fww(i,j) = ((306-j)*ww11(i-296, j-148) + (j-296)*ww7(i-296, j-296))/10.
    ftemp(i,j) = ((306-j)*ftemp11(i-296, j-148) + (j-296)*ftemp7(i-296, j-296))/10.
  ENDDO
ENDDO

!! Y2 Square Third (11,12,7,8), i = 445..454
DO j=297,306
  wyS=(306-j)/10.
  wyN=(j-296)/10.
  DO i=445,454
    wxW=(454-i)/10.
    wxE=(i-444)/10.
    fuu(i,j)= wxW*wyS*uu11(i-296, j-148) + wxE*wyS*uu12(i-444, j-148) + wxW*wyN*uu7 (i-296, j-296) + wxE*wyN*uu8 (i-444, j-296)
    fvv(i,j)= wxW*wyS*vv11(i-296, j-148) + wxE*wyS*vv12(i-444, j-148) + wxW*wyN*vv7 (i-296, j-296) + wxE*wyN*vv8 (i-444, j-296)
    fww(i,j)= wxW*wyS*ww11(i-296, j-148) + wxE*wyS*ww12(i-444, j-148) + wxW*wyN*ww7 (i-296, j-296) + wxE*wyN*ww8 (i-444, j-296)
    ftemp(i,j)= wxW*wyS*ftemp11(i-296, j-148) + wxE*wyS*ftemp12(i-444, j-148) + wxW*wyN*ftemp7 (i-296, j-296) + wxE*wyN*ftemp8 (i-444, j-296)
  ENDDO
ENDDO

!! Y2 Fourth Buffer (12-8), i = 455..602
DO j=297,306
  DO i=455,602
    fuu(i,j) = ((306-j)*uu12(i-444, j-148) + (j-296)*uu8(i-444, j-296))/10.
    fvv(i,j) = ((306-j)*vv12(i-444, j-148) + (j-296)*vv8(i-444, j-296))/10.
    fww(i,j) = ((306-j)*ww12(i-444, j-148) + (j-296)*ww8(i-444, j-296))/10.
    ftemp(i,j) = ((306-j)*ftemp12(i-444, j-148) + (j-296)*ftemp8(i-444, j-296))/10.
  ENDDO
ENDDO


!========================
! 중상단 행 (D05 ~ D08)  j = 307..444
!========================

! Copy 5
DO j=307,444
  DO i=1,148
    fuu(i,j) = uu5(i, j-296)
    fvv(i,j) = vv5(i, j-296)
    fww(i,j) = ww5(i, j-296)
    ftemp(i,j) = ftemp5(i, j-296)
  ENDDO
ENDDO

!! X1 Buffer (5-6): i = 149..158
DO j=307,444
  DO i=149,158
    fuu(i,j) = ((158-i)*uu5(i, j-296)   + (i-148)*uu6(i-148, j-296))/10.
    fvv(i,j) = ((158-i)*vv5(i, j-296)   + (i-148)*vv6(i-148, j-296))/10.
    fww(i,j) = ((158-i)*ww5(i, j-296)   + (i-148)*ww6(i-148, j-296))/10.
    ftemp(i,j) = ((158-i)*ftemp5(i, j-296)   + (i-148)*ftemp6(i-148, j-296))/10.
  ENDDO
ENDDO

! Copy 6: i = 159..296
DO j=307,444
  DO i=159,296
    fuu(i,j) = uu6(i-148, j-296)
    fvv(i,j) = vv6(i-148, j-296)
    fww(i,j) = ww6(i-148, j-296)
    ftemp(i,j) = ftemp6(i-148, j-296)
  ENDDO
ENDDO

!! X2 Buffer (6-7): i = 297..306
DO j=307,444
  DO i=297,306
    fuu(i,j) = ((306-i)*uu6(i-148, j-296) + (i-296)*uu7(i-296, j-296))/10.
    fvv(i,j) = ((306-i)*vv6(i-148, j-296) + (i-296)*vv7(i-296, j-296))/10.
    fww(i,j) = ((306-i)*ww6(i-148, j-296) + (i-296)*ww7(i-296, j-296))/10.
    ftemp(i,j) = ((306-i)*ftemp6(i-148, j-296) + (i-296)*ftemp7(i-296, j-296))/10.
  ENDDO
ENDDO

! Copy 7: i = 307..444
DO j=307,444
  DO i=307,444
    fuu(i,j) = uu7(i-296, j-296)
    fvv(i,j) = vv7(i-296, j-296)
    fww(i,j) = ww7(i-296, j-296)
    ftemp(i,j) = ftemp7(i-296, j-296)
  ENDDO
ENDDO

!! X3 Buffer (7-8): i = 445..454
DO j=307,444
  DO i=445,454
    fuu(i,j) = ((454-i)*uu7(i-296, j-296) + (i-444)*uu8(i-444, j-296))/10.
    fvv(i,j) = ((454-i)*vv7(i-296, j-296) + (i-444)*vv8(i-444, j-296))/10.
    fww(i,j) = ((454-i)*ww7(i-296, j-296) + (i-444)*ww8(i-444, j-296))/10.
    ftemp(i,j) = ((454-i)*ftemp7(i-296, j-296) + (i-444)*ftemp8(i-444, j-296))/10.
  ENDDO
ENDDO

! Copy 8: i = 455..602
DO j=307,444
  DO i=455,602
    fuu(i,j) = uu8(i-444, j-296)
    fvv(i,j) = vv8(i-444, j-296)
    fww(i,j) = ww8(i-444, j-296)
    ftemp(i,j) = ftemp8(i-444, j-296)
  ENDDO
ENDDO


!========================================
! Y3 버퍼 (D05~D08 ↔ D01~D04), j = 445..454
!========================================

!! Y3 First Buffer (5-1), i = 1..148
DO j=445,454
  DO i=1,148
    fuu(i,j) = ((454-j)*uu5(i, j-296)   + (j-444)*uu1(i, j-444))/10.
    fvv(i,j) = ((454-j)*vv5(i, j-296)   + (j-444)*vv1(i, j-444))/10.
    fww(i,j) = ((454-j)*ww5(i, j-296)   + (j-444)*ww1(i, j-444))/10.
    ftemp(i,j) = ((454-j)*ftemp5(i, j-296)   + (j-444)*ftemp1(i, j-444))/10.
  ENDDO
ENDDO

!! Y3 Square First (5,6,1,2), i = 149..158
DO j=445,454
  wyS=(454-j)/10.
  wyN=(j-444)/10.
  DO i=149,158
    wxW=(158-i)/10.
    wxE=(i-148)/10.
    fuu(i,j)= wxW*wyS*uu5(i,    j-296) + wxE*wyS*uu6(i-148, j-296) + wxW*wyN*uu1(i,    j-444) + wxE*wyN*uu2(i-148, j-444)
    fvv(i,j)= wxW*wyS*vv5(i,    j-296) + wxE*wyS*vv6(i-148, j-296) + wxW*wyN*vv1(i,    j-444) + wxE*wyN*vv2(i-148, j-444)
    fww(i,j)= wxW*wyS*ww5(i,    j-296) + wxE*wyS*ww6(i-148, j-296) + wxW*wyN*ww1(i,    j-444) + wxE*wyN*ww2(i-148, j-444)
    ftemp(i,j)= wxW*wyS*ftemp5(i,    j-296) + wxE*wyS*ftemp6(i-148, j-296) + wxW*wyN*ftemp1(i,    j-444) + wxE*wyN*ftemp2(i-148, j-444)
  ENDDO
ENDDO

!! Y3 Second Buffer (6-2), i = 159..296
DO j=445,454
  DO i=159,296
    fuu(i,j) = ((454-j)*uu6(i-148, j-296) + (j-444)*uu2(i-148, j-444))/10.
    fvv(i,j) = ((454-j)*vv6(i-148, j-296) + (j-444)*vv2(i-148, j-444))/10.
    fww(i,j) = ((454-j)*ww6(i-148, j-296) + (j-444)*ww2(i-148, j-444))/10.
    ftemp(i,j) = ((454-j)*ftemp6(i-148, j-296) + (j-444)*ftemp2(i-148, j-444))/10.
  ENDDO
ENDDO

!! Y3 Square Second (6,7,2,3), i = 297..306
DO j=445,454
  wyS=(454-j)/10.
  wyN=(j-444)/10.
  DO i=297,306
    wxW=(306-i)/10.
    wxE=(i-296)/10.
    fuu(i,j)= wxW*wyS*uu6(i-148, j-296) + wxE*wyS*uu7(i-296, j-296) + wxW*wyN*uu2(i-148, j-444) + wxE*wyN*uu3(i-296, j-444)
    fvv(i,j)= wxW*wyS*vv6(i-148, j-296) + wxE*wyS*vv7(i-296, j-296) + wxW*wyN*vv2(i-148, j-444) + wxE*wyN*vv3(i-296, j-444)
    fww(i,j)= wxW*wyS*ww6(i-148, j-296) + wxE*wyS*ww7(i-296, j-296) + wxW*wyN*ww2(i-148, j-444) + wxE*wyN*ww3(i-296, j-444)
    ftemp(i,j)= wxW*wyS*ftemp6(i-148, j-296) + wxE*wyS*ftemp7(i-296, j-296) + wxW*wyN*ftemp2(i-148, j-444) + wxE*wyN*ftemp3(i-296, j-444)
  ENDDO
ENDDO

!! Y3 Third Buffer (7-3), i = 307..444
DO j=445,454
  DO i=307,444
    fuu(i,j) = ((454-j)*uu7(i-296, j-296) + (j-444)*uu3(i-296, j-444))/10.
    fvv(i,j) = ((454-j)*vv7(i-296, j-296) + (j-444)*vv3(i-296, j-444))/10.
    fww(i,j) = ((454-j)*ww7(i-296, j-296) + (j-444)*ww3(i-296, j-444))/10.
    ftemp(i,j) = ((454-j)*ftemp7(i-296, j-296) + (j-444)*ftemp3(i-296, j-444))/10.
  ENDDO
ENDDO

!! Y3 Square Third (7,8,3,4), i = 445..454
DO j=445,454
  wyS=(454-j)/10.
  wyN=(j-444)/10.
  DO i=445,454
    wxW=(454-i)/10.
    wxE=(i-444)/10.
    fuu(i,j)= wxW*wyS*uu7(i-296, j-296) + wxE*wyS*uu8(i-444, j-296) + wxW*wyN*uu3(i-296, j-444) + wxE*wyN*uu4(i-444, j-444)
    fvv(i,j)= wxW*wyS*vv7(i-296, j-296) + wxE*wyS*vv8(i-444, j-296) + wxW*wyN*vv3(i-296, j-444) + wxE*wyN*vv4(i-444, j-444)
    fww(i,j)= wxW*wyS*ww7(i-296, j-296) + wxE*wyS*ww8(i-444, j-296) + wxW*wyN*ww3(i-296, j-444) + wxE*wyN*ww4(i-444, j-444)
    ftemp(i,j)= wxW*wyS*ftemp7(i-296, j-296) + wxE*wyS*ftemp8(i-444, j-296) + wxW*wyN*ftemp3(i-296, j-444) + wxE*wyN*ftemp4(i-444, j-444)
  ENDDO
ENDDO

!! Y3 Fourth Buffer (8-4), i = 455..602
DO j=445,454
  DO i=455,602
    fuu(i,j) = ((454-j)*uu8(i-444, j-296) + (j-444)*uu4(i-444, j-444))/10.
    fvv(i,j) = ((454-j)*vv8(i-444, j-296) + (j-444)*vv4(i-444, j-444))/10.
    fww(i,j) = ((454-j)*ww8(i-444, j-296) + (j-444)*ww4(i-444, j-444))/10.
    ftemp(i,j) = ((454-j)*ftemp8(i-444, j-296) + (j-444)*ftemp4(i-444, j-444))/10.
  ENDDO
ENDDO


!========================
! 상단 행 (D01 ~ D04)  j = 455..602
!========================

! Copy 1
DO j=455,602
  DO i=1,148
    fuu(i,j) = uu1(i, j-444)
    fvv(i,j) = vv1(i, j-444)
    fww(i,j) = ww1(i, j-444)
    ftemp(i,j) = ftemp1(i, j-444)
  ENDDO
ENDDO

!! X1 Buffer (1-2): i = 149..158
DO j=455,602
  DO i=149,158
    fuu(i,j) = ((158-i)*uu1(i, j-444)   + (i-148)*uu2(i-148, j-444))/10.
    fvv(i,j) = ((158-i)*vv1(i, j-444)   + (i-148)*vv2(i-148, j-444))/10.
    fww(i,j) = ((158-i)*ww1(i, j-444)   + (i-148)*ww2(i-148, j-444))/10.
    ftemp(i,j) = ((158-i)*ftemp1(i, j-444)   + (i-148)*ftemp2(i-148, j-444))/10.
  ENDDO
ENDDO

! Copy 2: i = 159..296
DO j=455,602
  DO i=159,296
    fuu(i,j) = uu2(i-148, j-444)
    fvv(i,j) = vv2(i-148, j-444)
    fww(i,j) = ww2(i-148, j-444)
    ftemp(i,j) = ftemp2(i-148, j-444)
  ENDDO
ENDDO

!! X2 Buffer (2-3): i = 297..306
DO j=455,602
  DO i=297,306
    fuu(i,j) = ((306-i)*uu2(i-148, j-444) + (i-296)*uu3(i-296, j-444))/10.
    fvv(i,j) = ((306-i)*vv2(i-148, j-444) + (i-296)*vv3(i-296, j-444))/10.
    fww(i,j) = ((306-i)*ww2(i-148, j-444) + (i-296)*ww3(i-296, j-444))/10.
    ftemp(i,j) = ((306-i)*ftemp2(i-148, j-444) + (i-296)*ftemp3(i-296, j-444))/10.
  ENDDO
ENDDO

! Copy 3: i = 307..444
DO j=455,602
  DO i=307,444
    fuu(i,j) = uu3(i-296, j-444)
    fvv(i,j) = vv3(i-296, j-444)
    fww(i,j) = ww3(i-296, j-444)
    ftemp(i,j) = ftemp3(i-296, j-444)
  ENDDO
ENDDO

!! X3 Buffer (3-4): i = 445..454
DO j=455,602
  DO i=445,454
    fuu(i,j) = ((454-i)*uu3(i-296, j-444) + (i-444)*uu4(i-444, j-444))/10.
    fvv(i,j) = ((454-i)*vv3(i-296, j-444) + (i-444)*vv4(i-444, j-444))/10.
    fww(i,j) = ((454-i)*ww3(i-296, j-444) + (i-444)*ww4(i-444, j-444))/10.
    ftemp(i,j) = ((454-i)*ftemp3(i-296, j-444) + (i-444)*ftemp4(i-444, j-444))/10.
  ENDDO
ENDDO

! Copy 4: i = 455..602
DO j=455,602
  DO i=455,602
    fuu(i,j) = uu4(i-444, j-444)
    fvv(i,j) = vv4(i-444, j-444)
    fww(i,j) = ww4(i-444, j-444)
    ftemp(i,j) = ftemp4(i-444, j-444)
  ENDDO
ENDDO

!!!!!!!! Integral D01 ~ D16 !!!!!!!!!!
      do 360 j= 1,ejmax2
      do 360 i= 1,eimax2
      ffuu(i,j) = fuu(i,j) 
      ffvv(i,j) = fvv(i,j)
      ffww(i,j) = fww(i,j)
      fftemp(i,j) = ftemp(i,j)
  360 continue

      do 370 j= 1,ejmax2
      do 370 i= 1,eimax2
       write(99,365) ffuu(i,j), ffvv(i,j), ffww(i,j), fftemp(i,j)
  370 continue

     WRITE(*,'(A,I2,A,I4.4,A,I2.2,A,I2.2,A,I2.2,A)') &
          'Layer = ',kk,', Time = ',sc_yy,'-',sc_mm,'-',sc_dd,' ',sc_hh,' UTC'

      ENDDO  !! kk

      CLOSE(99)

  365 format(4(1x,f12.6))
  366 format(8(1x,f12.6))
!c############################################################################
      stop
      end program E_SOFTCUBE
