C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00001
C .                                                                   . STA00002
C .                              S T A P                              . STA00003
C .                                                                   . STA00004
C .            AN IN-CORE SOLUTION STATIC ANALYSIS PROGRAM            . STA00005
C .                                                                   . STA00006
C . . . . . . . . . . . . . .  . . .  . . . . . . . . . . . . . . . . . STA00007
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                STA00008
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15   STA00009
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             STA00010
      COMMON /VAR/ NG,MODEX                                             STA00011
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00012
C                                                                       STA00013
      DIMENSION TIM(5), HED(20)                                         STA00014
      DIMENSION IA(1)                                                   STA00015
      EQUIVALENCE (A(1),IA(1))                                          STA00016
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00017
C .   THE FOLLOWING TWO LINES ARE USED TO DETERMINE THE MAXIMUM HIGH  . STA00018
C .   SPEED STORAGE THAT CAN BE USED FOR SOLUTION. TO CHANGE THE HIGH . STA00019
C .   SPEED STORAGE AVAILABLE FOR EXECUTION, CHANGE THE VALUE OF MTOT . STA00020
C .   AND CORRESPONDINGLY COMMON A(MTOT).                             . STA00021
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00022
      COMMON A(10000)                                                   STA00023
      MTOT=10000                                                        STA00024
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00025
C .   DOUBLE PRECISION LINE                                           . STA00026
C .     ITWO = 1 SINGLE PRECISION ARITHMETIC                          . STA00027
C .     ITWO = 2 DOUBLE PRECISION ARITHMETIC                          . STA00028
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00029
      ITWO=2                                                            STA00030
C                                                                       STA00031
C     THE FOLLOWING SCRATCH FILES ARE USED                              STA00032
C        IELMNT = UNIT STORING ELEMENT DATA                             STA00033
C        ILOAD  = UNIT STORING LOAD VECTORS                             STA00034
C        IIN    = UNIT USED FOR INPUT                                   STA00035
C        IOUT   = UNIT USED FOR OUTPUT                                  STA00036
C                                                                       STA00037
C     ON SOME MACHINES THESE FILES MUST BE EXPLICITLY OPENED            STA00038
C                                                                       STA00039
      IELMNT = 1                                                        STA00040
      ILOAD = 2                                                         STA00041
      IIN = 5                                                           STA00042
      IOUT = 6                                                          STA00043
C                                                                       STA00044
  200 NUMEST=0                                                          STA00045
      MAXEST=0                                                          STA00046
C     * * * * * * * * * * * * * * * * * * * * * *                       STA00047
C                                                                       STA00048
C     * * *   I N P U T   P H A S E   * * *                             STA00049
C                                                                       STA00050
C     * * * * * * * * * * * * * * * * * * * * * *                       STA00051
      CALL SECOND (TIM(1))                                              STA00052
C                                                                       STA00053
C                                                                       STA00054
C     R E A D   C O N T R O L   I N F O R M A T I O N                   STA00055
C                                                                       STA00056
C                                                                       STA00057
      READ (IIN,1000) HED,NUMNP,NUMEG,NLCASE,MODEX                      STA00058
      IF (NUMNP.EQ.0) GO TO 800                                         STA00059
      WRITE (IOUT,2000) HED,NUMNP,NUMEG,NLCASE,MODEX                    STA00060
C                                                                       STA00061
C                                                                       STA00062
C     R E A D   N O D A L   P O I N T   D A T A                         STA00063
C                                                                       STA00064
C                                                                       STA00065
      N1= 1                                                             STA00066
      N2=N1 + 3*NUMNP                                                   STA00067
      N2=(N2/2)*2 + 1                                                   STA00068
      N3=N2 + NUMNP*ITWO                                                STA00069
      N4=N3 + NUMNP*ITWO                                                STA00070
      N5=N4 + NUMNP*ITWO                                                STA00071
      IF (N5.GT.MTOT) CALL ERROR (N5-MTOT,1)                            STA00072
C                                                                       STA00073
      CALL INPUT (A(N1),A(N2),A(N3),A(N4),NUMNP,NEQ)                    STA00074
C                                                                       STA00075
      NEQ1=NEQ + 1                                                      STA00076
C                                                                       STA00077
C                                                                       STA00078
C     C A L C U L A T E   A N D   S T O R E   L O A D   V E C T O R S   STA00079
C                                                                       STA00080
C                                                                       STA00081
      N6=N5 + NEQ*ITWO                                                  STA00082
      WRITE (IOUT,2005)                                                 STA00083
C                                                                       STA00084
      REWIND ILOAD                                                      STA00085
C                                                                       STA00086
      DO 300 L=1,NLCASE                                                 STA00087
C                                                                       STA00088
      READ (IIN,1010) LL,NLOAD                                          STA00089
C                                                                       STA00090
      WRITE (IOUT,2010) LL,NLOAD                                        STA00091
      IF (LL.EQ.L) GO TO 310                                            STA00092
      WRITE (IOUT,2020)                                                 STA00093
      GO TO 800                                                         STA00094
  310 CONTINUE                                                          STA00095
C                                                                       STA00096
      N7=N6 + NLOAD                                                     STA00097
      N8=N7 + NLOAD                                                     STA00098
      N9=N8 + NLOAD*ITWO                                                STA00099
C                                                                       STA00100
      IF (N9.GT.MTOT) CALL ERROR (N9-MTOT,2)                            STA00101
C                                                                       STA00102
      CALL LOADS (A(N5),A(N6),A(N7),A(N8),A(N1),NLOAD,NEQ)              STA00103
C                                                                       STA00104
  300 CONTINUE                                                          STA00105
C                                                                       STA00106
C                                                                       STA00107
C     R E A D , G E N E R A T E   A N D   S T O R E                     STA00108
C     E L E M E N T   D A T A                                           STA00109
C                                                                       STA00110
C     CLEAR STORAGE                                                     STA00111
C                                                                       STA00112
      N6=N5 + NEQ                                                       STA00113
      N6=(N6/2)*2 + 1                                                   STA00114
      DO 10 I=N5,N6                                                     STA00115
   10 IA(I)=0                                                           STA00116
      IND=1                                                             STA00117
C                                                                       STA00118
      CALL ELCAL                                                        STA00119
C                                                                       STA00120
      CALL SECOND (TIM(2))                                              STA00121
C     * * * * * * * * * * * * * * * * * * * * * *                       STA00122
C                                                                       STA00123
C     * * *   S O L U T I O N   P H A S E   * * *                       STA00124
C                                                                       STA00125
C     * * * * * * * * * * * * * * * * * * * * * *                       STA00126
C                                                                       STA00127
C     A S S E M B L E   S T I F F N E S S   M A T R I X                 STA00128
C                                                                       STA00129
C                                                                       STA00130
      CALL ADDRES (A(N2),A(N5))                                         STA00131
C                                                                       STA00132
      MM=NWK/NEQ                                                        STA00133
      N3=N2 + NEQ + 1                                                   STA00134
      N3=(N3/2)*2 + 1                                                   STA00135
      N4=N3 + NWK*ITWO                                                  STA00136
      N5=N4 + NEQ*ITWO                                                  STA00137
      N6=N5 + MAXEST                                                    STA00138
      IF (N6.GT.MTOT) CALL ERROR (N6-MTOT,4)                            STA00139
C                                                                       STA00140
C     WRITE TOTAL SYSTEM DATA                                           STA00141
C                                                                       STA00142
      WRITE (IOUT,2025) NEQ,NWK,MK,MM                                   STA00143
C                                                                       STA00144
C     IN DATA CHECK ONLY MODE WE SKIP ALL FURTHER CALCULATIONS          STA00145
C                                                                       STA00146
      IF (MODEX.GT.0) GO TO 100                                         STA00147
      CALL SECOND (TIM(3))                                              STA00148
      CALL SECOND (TIM(4))                                              STA00149
      CALL SECOND (TIM(5))                                              STA00150
      GO TO 120                                                         STA00151
C                                                                       STA00152
C     CLEAR STORAGE                                                     STA00153
C                                                                       STA00154
  100 NNL=NWK + NEQ                                                     STA00155
      CALL CLEAR (A(N3),NNL)                                            STA00156
C                                                                       STA00157
      IND=2                                                             STA00158
C                                                                       STA00159
      CALL ASSEM (A(N5))                                                STA00160
C                                                                       STA00161
      CALL SECOND (TIM(3))                                              STA00162
C                                                                       STA00163
C                                                                       STA00164
C     T R I A N G U L A R I Z E   S T I F F N E S S   M A T R I X       STA00165
C                                                                       STA00166
C                                                                       STA00167
      KTR=1                                                             STA00168
      CALL COLSOL (A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,KTR)                  STA00169
C                                                                       STA00170
   35 CALL SECOND (TIM(4))                                              STA00171
C                                                                       STA00172
      KTR=2                                                             STA00173
      IND=3                                                             STA00174
C                                                                       STA00175
      REWIND ILOAD                                                      STA00176
      DO 400 L=1,NLCASE                                                 STA00177
C                                                                       STA00178
      CALL LOADV (A(N4),NEQ)                                            STA00179
C                                                                       STA00180
C                                                                       STA00181
C     C A L C U L A T I O N   O F   D I S P L A C E M E N T S           STA00182
C                                                                       STA00183
C                                                                       STA00184
      CALL COLSOL (A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,KTR)                  STA00185
C                                                                       STA00186
      WRITE (IOUT,2015) L                                               STA00187
      CALL WRITED (A(N4),A(N1),NEQ,NUMNP)                               STA00188
C                                                                       STA00189
C                                                                       STA00190
C     C A L C U L A T I O N   O F   S T R E S S E S                     STA00191
C                                                                       STA00192
C                                                                       STA00193
      CALL STRESS (A(N5))                                               STA00194
C                                                                       STA00195
  400 CONTINUE                                                          STA00196
C                                                                       STA00197
      CALL SECOND (TIM(5))                                              STA00198
C                                                                       STA00199
C     PRINT SOLUTION TIMES                                              STA00200
C                                                                       STA00201
  120 TT=0.                                                             STA00202
      DO 500 I=1,4                                                      STA00203
      TIM(I)=TIM(I+1) - TIM(I)                                          STA00204
  500 TT=TT + TIM(I)                                                    STA00205
      WRITE (IOUT,2030) HED,(TIM(I),I=1,4),TT                           STA00206
C                                                                       STA00207
C     READ NEXT ANALYSIS CASE                                           STA00208
C                                                                       STA00209
      GO TO 200                                                         STA00210
C                                                                       STA00211
  800 STOP                                                              STA00212
C                                                                       STA00213
 1000 FORMAT (20A4,/,4I5)                                               STA00214
 1010 FORMAT (2I5)                                                      STA00215
C                                                                       STA00216
 2000 FORMAT (///,' ',20A4,///,                                         STA00217
     1    ' C O N T R O L   I N F O R M A T I O N',//,                  STA00218
     2    '      NUMBER OF NODAL POINTS',10(' .'),' (NUMNP)  = ',I5,//, STA00219
     3    '      NUMBER OF ELEMENT GROUPS',9(' .'),' (NUMEG)  = ',I5,//,STA00220
     4    '      NUMBER OF LOAD CASES',11(' .'),' (NLCASE) = ',I5,//,   STA00221
     5    '      SOLUTION MODE ',14(' .'),' (MODEX)  = ',I5,/,          STA00222
     6    '         EQ.0, DATA CHECK',/,                                STA00223
     7    '         EQ.1, EXECUTION')                                   STA00224
 2005 FORMAT (//,' L O A D   C A S E   D A T A')                        STA00225
 2010 FORMAT (////,'     LOAD CASE NUMBER',7(' .'),' = ',I5,//,         STA00226
     1        '     NUMBER OF CONCENTRATED LOADS . = ',I5)              STA00227
 2015 FORMAT (//,' LOAD CASE ',I3)                                      STA00228
 2020 FORMAT (' *** ERROR *** LOAD CASES ARE NOT IN ORDER')             STA00229
 2025 FORMAT (//,' TOTAL SYSTEM DATA',///,                              STA00230
     1       '     NUMBER OF EQUATIONS',14(' .'),'(NEQ) = ',I5,//,      STA00231
     2       '     NUMBER OF MATRIX ELEMENTS',11(' .'),'(NWK) = ',I5,//,STA00232
     3       '     MAXIMUM HALF BANDWIDTH ',12(' .'),'(MK ) = ',I5,//,  STA00233
     4       '     MEAN HALF BANDWIDTH',14(' .'),'(MM ) = ',I5)         STA00234
 2030 FORMAT (//,' S O L U T I O N   T I M E   L O G   I N   S E C',//, STA00235
     1 '            FOR PROBLEM',//,' ',20A4,///,                       STA00236
     2 '     TIME FOR INPUT PHASE ',14(' .'),' =',F12.2,//,             STA00237
     3 '     TIME FOR CALCULATION OF STIFFNESS MATRIX  . . . . =',F12.2,STA00238
     4 //,                                                              STA00239
     5 '     TIME FOR FACTORIZATION OF STIFFNESS MATRIX  . . . =',F12.2,STA00240
     6 //,                                                              STA00241
     7 '     TIME FOR LOAD CASE SOLUTIONS ',10(' .'),' =',F12.2,///,    STA00242
     8 '      T O T A L   S O L U T I O N   T I M E  . . . . . =',F12.2)STA00243
C                                                                       STA00244
      END                                                               STA00245
      SUBROUTINE ERROR (N,I)                                            STA00246
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00247
C .                                                                   . STA00248
C .   P R O G R A M                                                   . STA00249
C .        TO PRINT MESSAGES WHEN HIGH-SPEED STORAGE IS EXCEEDED      . STA00250
C .                                                                   . STA00251
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00252
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00253
C                                                                       STA00254
      GO TO (1,2,3,4),I                                                 STA00255
C                                                                       STA00256
    1 WRITE (IOUT,2000)                                                 STA00257
      GO TO 6                                                           STA00258
    2 WRITE (IOUT,2010)                                                 STA00259
      GO TO 6                                                           STA00260
    3 WRITE (IOUT,2020)                                                 STA00261
      GO TO 6                                                           STA00262
    4 WRITE (IOUT,2030)                                                 STA00263
C                                                                       STA00264
    6 WRITE (IOUT,2050) N                                               STA00265
      STOP                                                              STA00266
C                                                                       STA00267
 2000 FORMAT (//,' NOT ENOUGH STORAGE FOR ID ARRAY AND NODAL POINT ',   STA00268
     1        'COORDINATES')                                            STA00269
 2010 FORMAT (//,' NOT ENOUGH STORAGE FOR DEFINITION OF LOAD VECTORS')  STA00270
 2020 FORMAT (//,' NOT ENOUGH STORAGE FOR ELEMENT DATA INPUT')          STA00271
 2030 FORMAT (//,' NOT ENOUGH STORAGE FOR ASSEMBLAGE OF GLOBAL ',       STA00272
     1'STRUCTURE STIFFNESS, AND DISPLACEMENT AND STRESS SOLUTION PHASE')STA00273
 2050 FORMAT (//,' *** ERROR ***  STORAGE EXCEEDED BY ', I9)            STA00274
C                                                                       STA00275
      END                                                               STA00276
      SUBROUTINE INPUT (ID,X,Y,Z,NUMNP,NEQ)                             STA00277
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00278
C .                                                                   . STA00279
C .   P R O G R A M                                                   . STA00280
C .      .TO READ, GENERATE, AND PRINT NODAL POINT INPUT DATA         . STA00281
C .      .TO CALCULATE EQUATION NUMBERS AND STORE THEM IN ID ARRRAY   . STA00282
C .                                                                   . STA00283
C .           N=ELEMENT NUMBER                                        . STA00284
C .           ID=BOUNDARY CONDITION CODES (0=FREE,1=DELETED)          . STA00285
C .           X,Y,Z= COORDINATES                                      . STA00286
C .           KN= GENERATION CODE                                     . STA00287
C .                    I.E. INCREMENT ON NODAL POINT NUMBER           . STA00288
C .                                                                   . STA00289
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00290
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               STA00291
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00292
C .   THE PROGRAM STAP IS USED IN SINGLE PRECISION ARITHMETIC ON CRAY . STA00293
C .   EQUIPMENT AND DOUBLE PRECISION ARITHMETIC ON IBM MACHINES,      . STA00294
C .   ENGINEERING WORKSTATIONS AND PCS. DEACTIVATE ABOVE LINE (ALSO   . STA00295
C .   OCCURRING IN OTHER SUBROUTINES) FOR SINGLE PRECISION ARITHMETIC.. STA00296
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00297
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00298
      DIMENSION X(1),Y(1),Z(1),ID(3,NUMNP)                              STA00299
C                                                                       STA00300
C     READ AND GENERATE NODAL POINT DATA                                STA00301
C                                                                       STA00302
      WRITE (IOUT,2000)                                                 STA00303
      WRITE (IOUT,2010)                                                 STA00304
      WRITE (IOUT,2020)                                                 STA00305
      KNOLD=0                                                           STA00306
      NOLD=0                                                            STA00307
C                                                                       STA00308
   10 READ (IIN,1000) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN               STA00309
      WRITE (IOUT,2030) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN             STA00310
      IF (KNOLD.EQ.0) GO TO 50                                          STA00311
      NUM=(N-NOLD)/KNOLD                                                STA00312
      NUMN=NUM - 1                                                      STA00313
      IF (NUMN.LT.1) GO TO 50                                           STA00314
      XNUM=NUM                                                          STA00315
      DX=(X(N)-X(NOLD))/XNUM                                            STA00316
      DY=(Y(N)-Y(NOLD))/XNUM                                            STA00317
      DZ=(Z(N)-Z(NOLD))/XNUM                                            STA00318
      K=NOLD                                                            STA00319
      DO 30 J=1,NUMN                                                    STA00320
      KK=K                                                              STA00321
      K=K + KNOLD                                                       STA00322
      X(K)=X(KK) + DX                                                   STA00323
      Y(K)=Y(KK) + DY                                                   STA00324
      Z(K)=Z(KK) + DZ                                                   STA00325
      DO 30 I=1,3                                                       STA00326
      ID(I,K)=ID(I,KK)                                                  STA00327
   30 CONTINUE                                                          STA00328
C                                                                       STA00329
   50 NOLD=N                                                            STA00330
      KNOLD=KN                                                          STA00331
      IF (N.NE.NUMNP) GO TO 10                                          STA00332
C                                                                       STA00333
C     WRITE COMPLETE NODAL DATA                                         STA00334
C                                                                       STA00335
      WRITE (IOUT,2015)                                                 STA00336
      WRITE (IOUT,2020)                                                 STA00337
      DO 200 N=1,NUMNP                                                  STA00338
  200 WRITE (IOUT,2030) N,(ID(I,N),I=1,3),X(N),Y(N),Z(N),KN             STA00339
C                                                                       STA00340
C     NUMBER UNKNOWNS                                                   STA00341
C                                                                       STA00342
      NEQ=0                                                             STA00343
      DO 100 N=1,NUMNP                                                  STA00344
      DO 100 I=1,3                                                      STA00345
      IF (ID(I,N)) 110,120,110                                          STA00346
  120 NEQ=NEQ + 1                                                       STA00347
      ID(I,N)=NEQ                                                       STA00348
      GO TO 100                                                         STA00349
  110 ID(I,N)=0                                                         STA00350
  100 CONTINUE                                                          STA00351
C                                                                       STA00352
C     WRITE EQUATION NUMBERS                                            STA00353
C                                                                       STA00354
      WRITE (IOUT,2040) (N,(ID(I,N),I=1,3),N=1,NUMNP)                   STA00355
C                                                                       STA00356
      RETURN                                                            STA00357
C                                                                       STA00358
 1000 FORMAT (4I5,3F10.0,I5)                                            STA00359
 2000 FORMAT(//,' N O D A L   P O I N T   D A T A',/)                   STA00360
 2010 FORMAT(' INPUT NODAL DATA',//)                                    STA00361
 2015 FORMAT(//,' GENERATED NODAL DATA',//)                             STA00362
 2020 FORMAT('  NODE',10X,'BOUNDARY',25X,'NODAL POINT',17X,'MESH',/,    STA00363
     1' NUMBER     CONDITION  CODES',21X,'COORDINATES',14X,'GENERATING',STA00364
     2/,77X,'CODE',/,                                                   STA00365
     315X,'X    Y    Z',15X,'X',12X,'Y',12X,'Z',10X,'KN')               STA00366
 2030 FORMAT (I5,6X,3I5,6X,3F13.3,3X,I6)                                STA00367
 2040 FORMAT(//,' EQUATION NUMBERS',//,'   NODE',9X,                    STA00368
     1 'DEGREES OF FREEDOM',/,'  NUMBER',//,                            STA00369
     2 '     N',13X,'X    Y    Z',/,(1X,I5,9X,3I5))                     STA00370
C                                                                       STA00371
      END                                                               STA00372
      SUBROUTINE LOADS (R,NOD,IDIRN,FLOAD,ID,NLOAD,NEQ)                 STA00373
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00374
C .                                                                   . STA00375
C .   P R O G R A M                                                   . STA00376
C .      . TO READ NODAL LOAD DATA                                    . STA00377
C .      . TO CALCULATE THE LOAD VECTOR R FOR EACH LOAD CASE AND      . STA00378
C .        WRITE ONTO UNIT ILOAD                                      . STA00379
C .                                                                   . STA00380
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00381
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               STA00382
      COMMON /VAR/ NG,MODEX                                             STA00383
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00384
      DIMENSION R(NEQ),NOD(1),IDIRN(1),FLOAD(1)                         STA00385
      DIMENSION ID(3,1)                                                 STA00386
C                                                                       STA00387
      WRITE (IOUT,2000)                                                 STA00388
      READ (IIN,1000) (NOD(I),IDIRN(I),FLOAD(I),I=1,NLOAD)              STA00389
      WRITE (IOUT,2010) (NOD(I),IDIRN(I),FLOAD(I),I=1,NLOAD)            STA00390
      IF (MODEX.EQ.0) GO TO 900                                         STA00391
C                                                                       STA00392
      DO 210 I=1,NEQ                                                    STA00393
  210 R(I)=0.                                                           STA00394
C                                                                       STA00395
      DO 220 L=1,NLOAD                                                  STA00396
      LN=NOD(L)                                                         STA00397
      LI=IDIRN(L)                                                       STA00398
      II=ID(LI,LN)                                                      STA00399
      IF (II) 220,220,240                                               STA00400
  240 R(II)=R(II) + FLOAD(L)                                            STA00401
C                                                                       STA00402
  220 CONTINUE                                                          STA00403
C                                                                       STA00404
      WRITE (ILOAD) R                                                   STA00405
C                                                                       STA00406
  200 CONTINUE                                                          STA00407
C                                                                       STA00408
  900 RETURN                                                            STA00409
C                                                                       STA00410
 1000 FORMAT (2I5,F10.0)                                                STA00411
 2000 FORMAT (//,'    NODE       DIRECTION      LOAD',/,                STA00412
     1        '   NUMBER',19X,'MAGNITUDE')                              STA00413
 2010 FORMAT (' ',I6,9X,I4,7X,E12.5)                                    STA00414
C                                                                       STA00415
      END                                                               STA00416
      SUBROUTINE ELCAL                                                  STA00417
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00418
C .                                                                   . STA00419
C .   P R O G R A M                                                   . STA00420
C .        TO LOOP OVER ALL ELEMENT GROUPS FOR READING,               . STA00421
C .        GENERATING AND STORING THE ELEMENT DATA                    . STA00422
C .                                                                   . STA00423
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00424
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                STA00425
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             STA00426
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00427
      COMMON A(1)                                                       STA00428
C                                                                       STA00429
      REWIND IELMNT                                                     STA00430
      WRITE (IOUT,2000)                                                 STA00431
C                                                                       STA00432
C     LOOP OVER ALL ELEMENT GROUPS                                      STA00433
C                                                                       STA00434
      DO 100 N=1,NUMEG                                                  STA00435
      IF (N.NE.1) WRITE (IOUT,2010)                                     STA00436
C                                                                       STA00437
      READ (IIN,1000) NPAR                                              STA00438
C                                                                       STA00439
      CALL ELEMNT                                                       STA00440
C                                                                       STA00441
      IF (MIDEST.GT.MAXEST) MAXEST=MIDEST                               STA00442
C                                                                       STA00443
      WRITE (IELMNT) MIDEST,NPAR,(A(I),I=NFIRST,NLAST)                  STA00444
C                                                                       STA00445
  100 CONTINUE                                                          STA00446
C                                                                       STA00447
      RETURN                                                            STA00448
C                                                                       STA00449
 1000 FORMAT (10I5)                                                     STA00450
 2000 FORMAT (//,' E L E M E N T   G R O U P   D A T A',//)             STA00451
 2010 FORMAT (' ')                                                      STA00452
C                                                                       STA00453
      END                                                               STA00454
      SUBROUTINE ELEMNT                                                 STA00455
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00456
C .                                                                   . STA00457
C .   P R O G R A M                                                   . STA00458
C .        TO CALL THE APPROPRIATE ELEMENT SUBROUTINE                 . STA00459
C .                                                                   . STA00460
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00461
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             STA00462
C                                                                       STA00463
      NPAR1=NPAR(1)                                                     STA00464
C                                                                       STA00465
      GO TO (1,2,3),NPAR1                                               STA00466
C                                                                       STA00467
    1 CALL TRUSS                                                        STA00468
      GO TO 900                                                         STA00469
C                                                                       STA00470
C     OTHER ELEMENT TYPES WOULD BE CALLED HERE, IDENTIFYING EACH        STA00471
C     ELEMENT TYPE BY A DIFFERENT NPAR(1) PARAMETER                     STA00472
C                                                                       STA00473
    2 GO TO 900                                                         STA00474
C                                                                       STA00475
    3 GO TO 900                                                         STA00476
C                                                                       STA00477
  900 RETURN                                                            STA00478
      END                                                               STA00479
      SUBROUTINE COLHT (MHT,ND,LM)                                      STA00480
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00481
C .                                                                   . STA00482
C .   P R O G R A M                                                   . STA00483
C .        TO CALCULATE COLUMN HEIGHTS                                . STA00484
C .                                                                   . STA00485
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00486
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                STA00487
      DIMENSION LM(1),MHT(1)                                            STA00488
C                                                                       STA00489
      LS=100000                                                         STA00490
      DO 100 I=1,ND                                                     STA00491
      IF (LM(I)) 110,100,110                                            STA00492
  110 IF (LM(I)-LS) 120,100,100                                         STA00493
  120 LS=LM(I)                                                          STA00494
  100 CONTINUE                                                          STA00495
C                                                                       STA00496
      DO 200 I=1,ND                                                     STA00497
      II=LM(I)                                                          STA00498
      IF (II.EQ.0) GO TO 200                                            STA00499
      ME=II - LS                                                        STA00500
      IF (ME.GT.MHT(II)) MHT(II)=ME                                     STA00501
 200  CONTINUE                                                          STA00502
C                                                                       STA00503
      RETURN                                                            STA00504
      END                                                               STA00505
      SUBROUTINE ADDRES (MAXA,MHT)                                      STA00506
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00507
C .                                                                   . STA00508
C .   P R O G R A M                                                   . STA00509
C .        TO CALCULATE ADDRESSES OF DIAGONAL ELEMENTS IN BANDED      . STA00510
C .        MATRIX WHOSE COLUMN HEIGHTS ARE KNOWN                      . STA00511
C .                                                                   . STA00512
C .        MHT  = ACTIVE COLUMN HEIGHTS                               . STA00513
C.         MAXA = ADDRESSES OF DIAGONAL ELEMENTS                      . STA00514
C .                                                                   . STA00515
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00516
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                STA00517
      DIMENSION MAXA(*),MHT(*)                                          STA00518
C                                                                       STA00519
C     CLEAR ARRAY MAXA                                                  STA00520
C                                                                       STA00521
      NN=NEQ + 1                                                        STA00522
      DO 20 I=1,NN                                                      STA00523
   20 MAXA(I)=0.0                                                       STA00524
C                                                                       STA00525
      MAXA(1)=1                                                         STA00526
      MAXA(2)=2                                                         STA00527
      MK=0                                                              STA00528
      IF (NEQ.EQ.1) GO TO 100                                           STA00529
      DO 10 I=2,NEQ                                                     STA00530
      IF (MHT(I).GT.MK) MK=MHT(I)                                       STA00531
   10 MAXA(I+1)=MAXA(I) + MHT(I) + 1                                    STA00532
  100 MK=MK + 1                                                         STA00533
      NWK=MAXA(NEQ+1) - MAXA(1)                                         STA00534
C                                                                       STA00535
      RETURN                                                            STA00536
      END                                                               STA00537
      SUBROUTINE CLEAR (A,N)                                            STA00538
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00539
C .                                                                   . STA00540
C .   P R O G R A M                                                   . STA00541
C .        TO CLEAR ARRAY A                                           . STA00542
C .                                                                   . STA00543
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00544
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               STA00545
      DIMENSION A(1)                                                    STA00546
      DO 10 I=1,N                                                       STA00547
   10 A(I)=0.                                                           STA00548
      RETURN                                                            STA00549
      END                                                               STA00550
      SUBROUTINE ASSEM (AA)                                             STA00551
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00552
C .                                                                   . STA00553
C .   P R O G R A M                                                   . STA00554
C .        TO CALL ELEMENT SUBROUTINES FOR ASSEMBLAGE OF THE          . STA00555
C .        STRUCTURE STIFFNESS MATRIX                                 . STA00556
C .                                                                   . STA00557
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00558
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             STA00559
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00560
      DIMENSION AA(1)                                                   STA00561
C                                                                       STA00562
      REWIND IELMNT                                                     STA00563
C                                                                       STA00564
      DO 200 N=1,NUMEG                                                  STA00565
      READ (IELMNT) NUMEST,NPAR,(AA(I),I=1,NUMEST)                      STA00566
C                                                                       STA00567
      CALL ELEMNT                                                       STA00568
C                                                                       STA00569
  200 CONTINUE                                                          STA00570
C                                                                       STA00571
      RETURN                                                            STA00572
      END                                                               STA00573
      SUBROUTINE ADDBAN (A,MAXA,S,LM,ND)                                STA00574
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00575
C .                                                                   . STA00576
C .   P R O G R A M                                                   . STA00577
C .        TO ASSEMBLE UPPER TRIANGULAR ELEMENT STIFFNESS INTO        . STA00578
C .        COMPACTED GLOBAL STIFFNESS                                 . STA00579
C .                                                                   . STA00580
C .         A = GLOBAL STIFFNESS                                      . STA00581
C .         S = ELEMENT STIFFNESS                                     . STA00582
C .         ND = DEGREES OF FREEDOM IN ELEMENT STIFFNESS              . STA00583
C .                                                                   . STA00584
C .                   S(1)        S(2)        S(3)        . . .       . STA00585
C .         S   =                 S(ND+1)     S(ND+2)     . . .       . STA00586
C .                                           S(2*ND)     . . .       . STA00587
C .                                                       . . .       . STA00588
C .                                                                   . STA00589
C .                                                                   . STA00590
C .                   A(1)        A(3)        A(6)        . . .       . STA00591
C .         A   =                 A(2)        A(5)        . . .       . STA00592
C .                                           A(4)        . . .       . STA00593
C .                                                       . . .       . STA00594
C .                                                                   . STA00595
C .                                                                   . STA00596
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00597
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               STA00598
      DIMENSION A(1),MAXA(1),S(1),LM(1)                                 STA00599
C                                                                       STA00600
      NDI=0                                                             STA00601
      DO 200 I=1,ND                                                     STA00602
      II=LM(I)                                                          STA00603
      IF (II) 200,200,100                                               STA00604
 100  MI=MAXA(II)                                                       STA00605
      KS=I                                                              STA00606
      DO 220 J=1,ND                                                     STA00607
      JJ=LM(J)                                                          STA00608
      IF (JJ) 220,220,110                                               STA00609
 110  IJ=II - JJ                                                        STA00610
      IF (IJ) 220,210,210                                               STA00611
 210  KK=MI + IJ                                                        STA00612
      KSS=KS                                                            STA00613
      IF (J.GE.I) KSS=J + NDI                                           STA00614
      A(KK)=A(KK) + S(KSS)                                              STA00615
  220 KS=KS + ND - J                                                    STA00616
  200 NDI=NDI + ND - I                                                  STA00617
C                                                                       STA00618
      RETURN                                                            STA00619
      END                                                               STA00620
      SUBROUTINE COLSOL (A,V,MAXA,NN,NWK,NNM,KKK)                       STA00621
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00622
C .                                                                   . STA00623
C .   P R O G R A M                                                   . STA00624
C .        TO SOLVE FINITE ELEMENT STATIC EQUILIBRIUM EQUATIONS IN    . STA00625
C .        CORE, USING COMPACTED STORAGE AND COLUMN REDUCTION SCHEME  . STA00626
C .                                                                   . STA00627
C .  - - INPUT VARIABLES - -                                          . STA00628
C .        A(NWK)    = STIFFNESS MATRIX STORED IN COMPACTED FORM      . STA00629
C .        V(NN)     = RIGHT-HAND-SIDE LOAD VECTOR                    . STA00630
C .        MAXA(NNM) = VECTOR CONTAINING ADDRESSES OF DIAGONAL        . STA00631
C .                    ELEMENTS OF STIFFNESS MATRIX IN A              . STA00632
C .        NN        = NUMBER OF EQUATIONS                            . STA00633
C .        NWK       = NUMBER OF ELEMENTS BELOW SKYLINE OF MATRIX     . STA00634
C .        NNM       = NN + 1                                         . STA00635
C .        KKK       = INPUT FLAG                                     . STA00636
C .            EQ. 1   TRIANGULARIZATION OF STIFFNESS MATRIX          . STA00637
C .            EQ. 2   REDUCTION AND BACK-SUBSTITUTION OF LOAD VECTOR . STA00638
C .        IOUT      = UNIT USED FOR OUTPUT                           . STA00639
C .                                                                   . STA00640
C .  - - OUTPUT - -                                                   . STA00641
C .        A(NWK)    = D AND L - FACTORS OF STIFFNESS MATRIX          . STA00642
C .        V(NN)     = DISPLACEMENT VECTOR                            . STA00643
C .                                                                   . STA00644
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00645
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               STA00646
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00647
      DIMENSION A(NWK),V(1),MAXA(1)                                     STA00648
C                                                                       STA00649
C     PERFORM L*D*L(T) FACTORIZATION OF STIFFNESS MATRIX                STA00650
C                                                                       STA00651
      IF (KKK-2) 40,150,150                                             STA00652
   40 DO 140 N=1,NN                                                     STA00653
      KN=MAXA(N)                                                        STA00654
      KL=KN + 1                                                         STA00655
      KU=MAXA(N+1) - 1                                                  STA00656
      KH=KU - KL                                                        STA00657
      IF (KH) 110,90,50                                                 STA00658
   50 K=N - KH                                                          STA00659
      IC=0                                                              STA00660
      KLT=KU                                                            STA00661
      DO 80 J=1,KH                                                      STA00662
      IC=IC + 1                                                         STA00663
      KLT=KLT - 1                                                       STA00664
      KI=MAXA(K)                                                        STA00665
      ND=MAXA(K+1) - KI - 1                                             STA00666
      IF (ND) 80,80,60                                                  STA00667
   60 KK=MIN0(IC,ND)                                                    STA00668
      C=0.                                                              STA00669
      DO 70 L=1,KK                                                      STA00670
   70 C=C + A(KI+L)*A(KLT+L)                                            STA00671
      A(KLT)=A(KLT) - C                                                 STA00672
   80 K=K + 1                                                           STA00673
   90 K=N                                                               STA00674
      B=0.                                                              STA00675
      DO 100 KK=KL,KU                                                   STA00676
      K=K - 1                                                           STA00677
      KI=MAXA(K)                                                        STA00678
      C=A(KK)/A(KI)                                                     STA00679
      B=B + C*A(KK)                                                     STA00680
  100 A(KK)=C                                                           STA00681
      A(KN)=A(KN) - B                                                   STA00682
  110 IF (A(KN)) 120,120,140                                            STA00683
  120 WRITE (IOUT,2000) N,A(KN)                                         STA00684
      GO TO 800                                                         STA00685
  140 CONTINUE                                                          STA00686
      GO TO 900                                                         STA00687
C                                                                       STA00688
C     REDUCE RIGHT-HAND-SIDE LOAD VECTOR                                STA00689
C                                                                       STA00690
  150 DO 180 N=1,NN                                                     STA00691
      KL=MAXA(N) + 1                                                    STA00692
      KU=MAXA(N+1) - 1                                                  STA00693
      IF (KU-KL) 180,160,160                                            STA00694
  160 K=N                                                               STA00695
      C=0.                                                              STA00696
      DO 170 KK=KL,KU                                                   STA00697
      K=K - 1                                                           STA00698
  170 C=C + A(KK)*V(K)                                                  STA00699
      V(N)=V(N) - C                                                     STA00700
  180 CONTINUE                                                          STA00701
C                                                                       STA00702
C     BACK-SUBSTITUTE                                                   STA00703
C                                                                       STA00704
      DO 200 N=1,NN                                                     STA00705
      K=MAXA(N)                                                         STA00706
  200 V(N)=V(N)/A(K)                                                    STA00707
      IF (NN.EQ.1) GO TO 900                                            STA00708
      N=NN                                                              STA00709
      DO 230 L=2,NN                                                     STA00710
      KL=MAXA(N) + 1                                                    STA00711
      KU=MAXA(N+1) - 1                                                  STA00712
      IF (KU-KL) 230,210,210                                            STA00713
  210 K=N                                                               STA00714
      DO 220 KK=KL,KU                                                   STA00715
      K=K - 1                                                           STA00716
  220 V(K)=V(K) - A(KK)*V(N)                                            STA00717
  230 N=N - 1                                                           STA00718
      GO TO 900                                                         STA00719
C                                                                       STA00720
  800 STOP                                                              STA00721
  900 RETURN                                                            STA00722
C                                                                       STA00723
 2000 FORMAT (//' STOP - STIFFNESS MATRIX NOT POSITIVE DEFINITE',//,    STA00724
     1          ' NONPOSITIVE PIVOT FOR EQUATION ',I8,//,               STA00725
     2          ' PIVOT = ',E20.12 )                                    STA00726
C                                                                       STA00727
      END                                                               STA00728
      SUBROUTINE LOADV (R,NEQ)                                          STA00729
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00730
C .                                                                   . STA00731
C .   P R O G R A M                                                   . STA00732
C .        TO OBTAIN THE LOAD VECTOR                                  . STA00733
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00734
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               STA00735
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00736
      DIMENSION R(NEQ)                                                  STA00737
C                                                                       STA00738
      READ (ILOAD) R                                                    STA00739
C                                                                       STA00740
      RETURN                                                            STA00741
      END                                                               STA00742
      SUBROUTINE WRITED (DISP,ID,NEQ,NUMNP)                             STA00743
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00744
C .                                                                   . STA00745
C .   P R O G R A M                                                   . STA00746
C .      TO PRINT DISPLACEMENTS                                       . STA00747
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00748
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               STA00749
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00750
      DIMENSION DISP(NEQ),ID(3,NUMNP)                                   STA00751
      DIMENSION D(3)                                                    STA00752
C                                                                       STA00753
C     PRINT DISPLACEMENTS                                               STA00754
C                                                                       STA00755
      WRITE (IOUT,2000)                                                 STA00756
      IC=4                                                              STA00757
C                                                                       STA00758
      DO 100 II=1,NUMNP                                                 STA00759
      IC=IC + 1                                                         STA00760
      IF (IC.LT.56) GO TO 105                                           STA00761
      WRITE (IOUT,2000)                                                 STA00762
      IC=4                                                              STA00763
  105 DO 110 I=1,3                                                      STA00764
  110 D(I)=0.                                                           STA00765
C                                                                       STA00766
      DO 120 I=1,3                                                      STA00767
      KK=ID(I,II)                                                       STA00768
      IL=I                                                              STA00769
  120 IF (KK.NE.0) D(IL)=DISP(KK)                                       STA00770
C                                                                       STA00771
  100 WRITE (IOUT,2010) II,D                                            STA00772
C                                                                       STA00773
      RETURN                                                            STA00774
C                                                                       STA00775
 2000 FORMAT (///,' D I S P L A C E M E N T S',//,'  NODE ',10X,        STA00776
     1        'X-DISPLACEMENT    Y-DISPLACEMENT    Z-DISPLACEMENT')     STA00777
 2010 FORMAT (1X,I3,8X,3E18.6)                                          STA00778
C                                                                       STA00779
      END                                                               STA00780
      SUBROUTINE STRESS (AA)                                            STA00781
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00782
C .                                                                   . STA00783
C .   P R O G R A M                                                   . STA00784
C .         TO CALL THE ELEMENT SUBROUTINE FOR THE CALCULATION OF     . STA00785
C .         STRESSES                                                  . STA00786
C .                                                                   . STA00787
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00788
      COMMON /VAR/ NG,MODEX                                             STA00789
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             STA00790
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00791
      DIMENSION AA(1)                                                   STA00792
C                                                                       STA00793
C     LOOP OVER ALL ELEMENT GROUPS                                      STA00794
C                                                                       STA00795
      REWIND IELMNT                                                     STA00796
C                                                                       STA00797
      DO 100 N=1,NUMEG                                                  STA00798
      NG=N                                                              STA00799
C                                                                       STA00800
      READ (IELMNT) NUMEST,NPAR,(AA(I),I=1,NUMEST)                      STA00801
C                                                                       STA00802
      CALL ELEMNT                                                       STA00803
C                                                                       STA00804
  100 CONTINUE                                                          STA00805
C                                                                       STA00806
      RETURN                                                            STA00807
      END                                                               STA00808
      SUBROUTINE TRUSS                                                  STA00809
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00810
C .                                                                   . STA00811
C .   P R O G R A M                                                   . STA00812
C .        TO SET UP STORAGE AND CALL THE TRUSS ELEMENT SUBROUTINE    . STA00813
C .                                                                   . STA00814
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00815
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                STA00816
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15   STA00817
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             STA00818
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00819
      COMMON A(1)                                                       STA00820
C                                                                       STA00821
      EQUIVALENCE (NPAR(2),NUME),(NPAR(3),NUMMAT)                       STA00822
C                                                                       STA00823
      NFIRST=N6                                                         STA00824
      IF (IND.GT.1) NFIRST=N5                                           STA00825
      N101=NFIRST                                                       STA00826
      N102=N101 + NUMMAT*ITWO                                           STA00827
      N103=N102 + NUMMAT*ITWO                                           STA00828
      N104=N103 + 6*NUME                                                STA00829
      N105=N104 + 6*NUME*ITWO                                           STA00830
      N106=N105 + NUME                                                  STA00831
      NLAST=N106                                                        STA00832
C                                                                       STA00833
      IF (IND.GT.1) GO TO 100                                           STA00834
      IF (NLAST.GT.MTOT) CALL ERROR (NLAST-MTOT,3)                      STA00835
      GO TO 200                                                         STA00836
  100 IF (NLAST.GT.MTOT) CALL ERROR (NLAST-MTOT,4)                      STA00837
C                                                                       STA00838
  200 MIDEST=NLAST - NFIRST                                             STA00839
C                                                                       STA00840
      CALL RUSS (A(N1),A(N2),A(N3),A(N4),A(N4),A(N5),A(N101),A(N102),   STA00841
     1 A(N103),A(N104),A(N105))                                         STA00842
C                                                                       STA00843
      RETURN                                                            STA00844
C                                                                       STA00845
      END                                                               STA00846
      SUBROUTINE RUSS (ID,X,Y,Z,U,MHT,E,AREA,LM,XYZ,MATP)               STA00847
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00848
C .                                                                   . STA00849
C .        TRUSS ELEMENT SUBROUTINE                                   . STA00850
C .                                                                   . STA00851
C . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . STA00852
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               STA00853
      REAL A                                                            STA00854
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK                STA00855
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15   STA00856
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO             STA00857
      COMMON /VAR/ NG,MODEX                                             STA00858
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT                              STA00859
      COMMON A(1)                                                       STA00860
C                                                                       STA00861
      DIMENSION X(1),Y(1),Z(1),ID(3,1),E(1),AREA(1),LM(6,1),            STA00862
     1          XYZ(6,1),MATP(1),U(1),MHT(1)                            STA00863
      DIMENSION S(21),ST(6),D(3)                                        STA00864
C                                                                       STA00865
      EQUIVALENCE (NPAR(1),NPAR1),(NPAR(2),NUME),(NPAR(3),NUMMAT)       STA00866
      ND=6                                                              STA00867
C                                                                       STA00868
      GO TO (300,610,800),IND                                           STA00869
C                                                                       STA00870
C                                                                       STA00871
C     R E A D   A N D   G E N E R A T E   E L E M E N T                 STA00872
C     I N F O R M A T I O N                                             STA00873
C                                                                       STA00874
C     READ MATERIAL INFORMATION                                         STA00875
C                                                                       STA00876
  300 WRITE (IOUT,2000) NPAR1,NUME                                      STA00877
      IF (NUMMAT.EQ.0) NUMMAT=1                                         STA00878
      WRITE (IOUT,2010) NUMMAT                                          STA00879
C                                                                       STA00880
      WRITE (IOUT,2020)                                                 STA00881
      DO 10 I=1,NUMMAT                                                  STA00882
      READ (IIN,1000) N,E(N),AREA(N)                                    STA00883
   10 WRITE (IOUT,2030) N,E(N),AREA(N)                                  STA00884
C                                                                       STA00885
C     READ ELEMENT INFORMATION                                          STA00886
C                                                                       STA00887
      WRITE (IOUT,2040)                                                 STA00888
      N=1                                                               STA00889
  100 READ (IIN,1020) M,II,JJ,MTYP,KG                                   STA00890
      IF (KG.EQ.0) KG=1                                                 STA00891
  120 IF (M.NE.N) GO TO 200                                             STA00892
      I=II                                                              STA00893
      J=JJ                                                              STA00894
      MTYPE=MTYP                                                        STA00895
      KKK=KG                                                            STA00896
C                                                                       STA00897
C     SAVE ELEMENT INFORMATION                                          STA00898
C                                                                       STA00899
  200 XYZ(1,N)=X(I)                                                     STA00900
      XYZ(2,N)=Y(I)                                                     STA00901
      XYZ(3,N)=Z(I)                                                     STA00902
C                                                                       STA00903
      XYZ(4,N)=X(J)                                                     STA00904
      XYZ(5,N)=Y(J)                                                     STA00905
      XYZ(6,N)=Z(J)                                                     STA00906
C                                                                       STA00907
      MATP(N)=MTYPE                                                     STA00908
C                                                                       STA00909
      DO 390 L=1,6                                                      STA00910
  390 LM(L,N)=0                                                         STA00911
      DO 400 L=1,3                                                      STA00912
      LM(L,N)=ID(L,I)                                                   STA00913
  400 LM(L+3,N)=ID(L,J)                                                 STA00914
C                                                                       STA00915
C     UPDATE COLUMN HEIGHTS AND BANDWIDTH                               STA00916
C                                                                       STA00917
      CALL COLHT (MHT,ND,LM(1,N))                                       STA00918
C                                                                       STA00919
      WRITE (IOUT,2050) N,I,J,MTYPE                                     STA00920
      IF (N.EQ.NUME) GO TO 900                                          STA00921
      N=N + 1                                                           STA00922
      I=I + KKK                                                         STA00923
      J=J + KKK                                                         STA00924
      IF (N.GT.M) GO TO 100                                             STA00925
      GO TO 120                                                         STA00926
C                                                                       STA00927
C                                                                       STA00928
C     A S S E M B L E  S T U C T U R E  S T I F F N E S S  M A T R I X  STA00929
C                                                                       STA00930
C                                                                       STA00931
  610 DO 500 N=1,NUME                                                   STA00932
      MTYPE=MATP(N)                                                     STA00933
      XL2=0.                                                            STA00934
      DO 505 L=1,3                                                      STA00935
      D(L)=XYZ(L,N) - XYZ(L+3,N)                                        STA00936
  505 XL2=XL2 + D(L)*D(L)                                               STA00937
      XL=SQRT(XL2)                                                      STA00938
      XX=E(MTYPE)*AREA(MTYPE)*XL                                        STA00939
      DO 510 L=1,3                                                      STA00940
      ST(L)=D(L)/XL2                                                    STA00941
  510 ST(L+3)=-ST(L)                                                    STA00942
C                                                                       STA00943
      KL=0                                                              STA00944
      DO 600 L=1,6                                                      STA00945
      YY=ST(L)*XX                                                       STA00946
      DO 600 K=L,6                                                      STA00947
      KL=KL + 1                                                         STA00948
  600 S(KL)=ST(K)*YY                                                    STA00949
      CALL ADDBAN (A(N3),A(N2),S,LM(1,N),ND)                            STA00950
  500 CONTINUE                                                          STA00951
      GO TO 900                                                         STA00952
C                                                                       STA00953
C                                                                       STA00954
C     S T R E S S  C A L C U L A T I O N S                              STA00955
C                                                                       STA00956
C                                                                       STA00957
  800 IPRINT=0                                                          STA00958
      DO 830 N=1,NUME                                                   STA00959
      IPRINT=IPRINT + 1                                                 STA00960
      IF (IPRINT.GT.50) IPRINT=1                                        STA00961
      IF (IPRINT.EQ.1) WRITE (IOUT,2060) NG                             STA00962
      MTYPE=MATP(N)                                                     STA00963
      XL2=0.                                                            STA00964
      DO 820 L=1,3                                                      STA00965
      D(L) = XYZ(L,N) - XYZ(L+3,N)                                      STA00966
  820 XL2=XL2 + D(L)*D(L)                                               STA00967
      DO 814 L=1,3                                                      STA00968
      ST(L)=(D(L)/XL2)*E(MTYPE)                                         STA00969
  814 ST(L+3)=-ST(L)                                                    STA00970
      STR=0.0                                                           STA00971
      DO 806 L=1,3                                                      STA00972
      I=LM(L,N)                                                         STA00973
      IF (I.LE.0) GO TO 807                                             STA00974
      STR=STR + ST(L)*U(I)                                              STA00975
  807 J=LM(L+3,N)                                                       STA00976
      IF (J.LE.0) GO TO 806                                             STA00977
      STR=STR + ST(L+3)*U(J)                                            STA00978
  806 CONTINUE                                                          STA00979
      P=STR*AREA(MTYPE)                                                 STA00980
      WRITE (IOUT,2070) N,P,STR                                         STA00981
  830 CONTINUE                                                          STA00982
C                                                                       STA00983
  900 RETURN                                                            STA00984
C                                                                       STA00985
 1000 FORMAT (I5,2F10.0)                                                STA00986
 1010 FORMAT (2F10.0)                                                   STA00987
 1020 FORMAT (5I5)                                                      STA00988
 2000 FORMAT (' E L E M E N T   D E F I N I T I O N',///,               STA00989
     1        ' ELEMENT TYPE ',13(' .'),'( NPAR(1) ) . . =',I5,/,       STA00990
     2        '     EQ.1, TRUSS ELEMENTS',/,                            STA00991
     3        '     EQ.2, ELEMENTS CURRENTLY',/,                        STA00992
     4        '     EQ.3, NOT AVAILABLE',//,                            STA00993
     5        ' NUMBER OF ELEMENTS.',10(' .'),'( NPAR(2) ) . . =',I5,//)STA00994
 2010 FORMAT (' M A T E R I A L   D E F I N I T I O N',///,             STA00995
     1        ' NUMBER OF DIFFERENT SETS OF MATERIAL',/,                STA00996
     2        ' AND CROSS-SECTIONAL  CONSTANTS ',                       STA00997
     3                           4(' .'),'( NPAR(3) ) . . =',I5,//)     STA00998
 2020 FORMAT ('  SET       YOUNG''S     CROSS-SECTIONAL',/,             STA00999
     1        ' NUMBER     MODULUS',10X,'AREA',/,                       STA01000
     2        15X,'E',14X,'A')                                          STA01001
 2030 FORMAT (/,I5,4X,E12.5,2X,E14.6)                                   STA01002
 2040 FORMAT (//,' E L E M E N T   I N F O R M A T I O N',///,          STA01003
     1        ' ELEMENT     NODE     NODE       MATERIAL',/,            STA01004
     2        ' NUMBER-N      I        J       SET NUMBER',/)           STA01005
 2050 FORMAT (I5,6X,I5,4X,I5,7X,I5)                                     STA01006
 2060 FORMAT (///,' S T R E S S  C A L C U L A T I O N S  F O R  ',     STA01007
     1        'E L E M E N T  G R O U P',I4,//,                         STA01008
     2        '  ELEMENT',13X,'FORCE',12X,'STRESS',/,                   STA01009
     3        '  NUMBER',/)                                             STA01010
 2070 FORMAT (1X,I5,11X,E13.6,4X,E13.6)                                 STA01011
C                                                                       STA01012
      END                                                               STA01013
      SUBROUTINE SECOND (TIM)                                           STA01014
C                                                                       STA01015
C     SUBROUTINE TO OBTAIN TIME                                         STA01016
C     THIS SUBROUTINE HAS BEEN USED ON AN IBM RS/6000 WORKSTATION       STA01017
C                                                                       STA01018
      TIM=0.01*MCLOCK()                                                 STA01019
C                                                                       STA01020
      RETURN                                                            STA01021
      END                                                               STA01022
