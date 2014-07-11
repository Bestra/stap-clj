      SUBROUTINE JACOBI (A,B,X,EIGV,D,N,RTOL,NSMAX,IFPR,IOUT)           JAC00001
C ..................................................................... JAC00002
C .                                                                   . JAC00003
C .   P R O G R A M                                                   . JAC00004
C .        TO SOLVE THE GENERALIZED EIGENPROBLEM USING THE            . JAC00005
C .        GENERALIZED JACOBI ITERATION                               . JAC00006
C .                                                                   . JAC00007
C . - - INPUT VARIABLES - -                                           . JAC00008
C .        A(N,N)    = STIFFNESS MATRIX (ASSUMED POSITIVE DEFINITE)   . JAC00009
C .        B(N,N)    = MASS MATRIX (ASSUMED POSITIVE DEFINITE)        . JAC00010
C .        X(N,N)    = STORAGE FOR EIGENVECTORS                       . JAC00011
C .        EIGV(N)   = STORAGE FOR EIGENVALUES                        . JAC00012
C .        D(N)      = WORKING VECTOR                                 . JAC00013
C .        N         = ORDER OF MATRICES A AND B                      . JAC00014
C .        RTOL      = CONVERGENCE TOLERANCE (USUALLY SET TO 10.**-12). JAC00015
C .        NSMAX     = MAXIMUM NUMBER OF SWEEPS ALLOWED               . JAC00016
C .                                  (USUALLY SET TO 15)              . JAC00017
C .        IFPR      = FLAG FOR PRINTING DURING ITERATION             . JAC00018
C .            EQ.0    NO PRINTING                                    . JAC00019
C .            EQ.1    INTERMEDIATE RESULTS ARE PRINTED               . JAC00020
C .        IOUT      = UNIT NUMBER USED FOR OUTPUT                    . JAC00021
C .                                                                   . JAC00022
C . - - OUTPUT - -                                                    . JAC00023
C .        A(N,N)    = DIAGONALIZED STIFFNESS MATRIX                  . JAC00024
C .        B(N,N)    = DIGONALIZED MASS MATRIX                        . JAC00025
C .        X(N,N)    = EIGENVECTORS STORED COLUMNWISE                 . JAC00026
C .        EIGV(N)   = EIGENVALUES                                    . JAC00027
C .                                                                   . JAC00028
C ..................................................................... JAC00029
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               JAC00030
C ..................................................................... JAC00031
C .   THIS PROGRAM IS USED IN SINGLE PRECISION ARITHMETIC ON CRAY     . JAC00032
C .   EQUIPMENT AND DOUBLE PRECISION ARITHMETIC ON IBM MACHINES,      . JAC00033
C .   ENGINEERING WORKSTATIONS AND PCS. DEACTIVATE ABOVE LINE FOR     . JAC00034
C .   SINGLE PRECISION ARITHMETIC.                                    . JAC00035
C ..................................................................... JAC00036
      DIMENSION A(N,N),B(N,N),X(N,N),EIGV(N),D(N)                       JAC00037
C                                                                       JAC00038
C     INITIALIZE EIGENVALUE AND EIGENVECTOR MATRICES                    JAC00039
C                                                                       JAC00040
      DO 10 I=1,N                                                       JAC00041
      IF (A(I,I).GT.0. .AND. B(I,I).GT.0.) GO TO 4                      JAC00042
      WRITE (IOUT,2020)                                                 JAC00043
      GO TO 800                                                         JAC00044
    4 D(I)=A(I,I)/B(I,I)                                                JAC00045
   10 EIGV(I)=D(I)                                                      JAC00046
      DO 30 I=1,N                                                       JAC00047
      DO 20 J=1,N                                                       JAC00048
   20 X(I,J)=0.                                                         JAC00049
   30 X(I,I)=1.                                                         JAC00050
      IF (N.EQ.1) GO TO 900                                             JAC00051
C                                                                       JAC00052
C     INITIALIZE SWEEP COUNTER AND BEGIN ITERATION                      JAC00053
C                                                                       JAC00054
      NSWEEP=0                                                          JAC00055
      NR=N - 1                                                          JAC00056
   40 NSWEEP=NSWEEP + 1                                                 JAC00057
      IF (IFPR.EQ.1) WRITE (IOUT,2000) NSWEEP                           JAC00058
C                                                                       JAC00059
C     CHECK IF PRESENT OFF-DIAGONAL ELEMENT IS LARGE ENOUGH TO          JAC00060
C     REQUIRE ZEROING                                                   JAC00061
C                                                                       JAC00062
      EPS=(.01)**(NSWEEP*2)                                             JAC00063
      DO 210 J=1,NR                                                     JAC00064
      JJ=J + 1                                                          JAC00065
      DO 210 K=JJ,N                                                     JAC00066
      EPTOLA=(A(J,K)/A(J,J))*(A(J,K)/A(K,K))                            JAC00067
      EPTOLB=(B(J,K)/B(J,J))*(B(J,K)/B(K,K))                            JAC00068
      IF (EPTOLA.LT.EPS .AND. EPTOLB.LT.EPS) GO TO 210                  JAC00069
C                                                                       JAC00070
C     IF ZEROING IS REQUIRED, CALCULATE THE ROTATION MATRIX             JAC00071
C     ELEMENTS CA AND CG                                                JAC00072
C                                                                       JAC00073
      AKK=A(K,K)*B(J,K) - B(K,K)*A(J,K)                                 JAC00074
      AJJ=A(J,J)*B(J,K) - B(J,J)*A(J,K)                                 JAC00075
      AB=A(J,J)*B(K,K) - A(K,K)*B(J,J)                                  JAC00076
      SCALE=A(K,K)*B(K,K)                                               JAC00077
      ABCH=AB/SCALE                                                     JAC00078
      AKKCH=AKK/SCALE                                                   JAC00079
      AJJCH=AJJ/SCALE                                                   JAC00080
      CHECK=(ABCH*ABCH + 4.*AKKCH*AJJCH)/4.                             JAC00081
      IF (CHECK) 50,60,60                                               JAC00082
   50 WRITE (IOUT,2020)                                                 JAC00083
      GO TO 800                                                         JAC00084
   60 SQCH=SCALE*SQRT(CHECK)                                            JAC00085
      D1=AB/2. + SQCH                                                   JAC00086
      D2=AB/2. - SQCH                                                   JAC00087
      DEN=D1                                                            JAC00088
      IF (ABS(D2).GT.ABS(D1)) DEN=D2                                    JAC00089
      IF (DEN) 80,70,80                                                 JAC00090
   70 CA=0.                                                             JAC00091
      CG=-A(J,K)/A(K,K)                                                 JAC00092
      GO TO 90                                                          JAC00093
   80 CA=AKK/DEN                                                        JAC00094
      CG=-AJJ/DEN                                                       JAC00095
C                                                                       JAC00096
C     PERFORM THE GENERALIZED ROTATION TO ZERO ELEMENTS                 JAC00097
C                                                                       JAC00098
   90 IF (N-2) 100,190,100                                              JAC00099
  100 JP1=J + 1                                                         JAC00100
      JM1=J - 1                                                         JAC00101
      KP1=K + 1                                                         JAC00102
      KM1=K - 1                                                         JAC00103
      IF (JM1-1) 130,110,110                                            JAC00104
  110 DO 120 I=1,JM1                                                    JAC00105
      AJ=A(I,J)                                                         JAC00106
      BJ=B(I,J)                                                         JAC00107
      AK=A(I,K)                                                         JAC00108
      BK=B(I,K)                                                         JAC00109
      A(I,J)=AJ + CG*AK                                                 JAC00110
      B(I,J)=BJ + CG*BK                                                 JAC00111
      A(I,K)=AK + CA*AJ                                                 JAC00112
  120 B(I,K)=BK + CA*BJ                                                 JAC00113
  130 IF (KP1-N) 140,140,160                                            JAC00114
  140 DO 150 I=KP1,N                                                    JAC00115
      AJ=A(J,I)                                                         JAC00116
      BJ=B(J,I)                                                         JAC00117
      AK=A(K,I)                                                         JAC00118
      BK=B(K,I)                                                         JAC00119
      A(J,I)=AJ + CG*AK                                                 JAC00120
      B(J,I)=BJ + CG*BK                                                 JAC00121
      A(K,I)=AK + CA*AJ                                                 JAC00122
  150 B(K,I)=BK + CA*BJ                                                 JAC00123
  160 IF (JP1-KM1) 170,170,190                                          JAC00124
  170 DO 180 I=JP1,KM1                                                  JAC00125
      AJ=A(J,I)                                                         JAC00126
      BJ=B(J,I)                                                         JAC00127
      AK=A(I,K)                                                         JAC00128
      BK=B(I,K)                                                         JAC00129
      A(J,I)=AJ + CG*AK                                                 JAC00130
      B(J,I)=BJ + CG*BK                                                 JAC00131
      A(I,K)=AK + CA*AJ                                                 JAC00132
  180 B(I,K)=BK + CA*BJ                                                 JAC00133
  190 AK=A(K,K)                                                         JAC00134
      BK=B(K,K)                                                         JAC00135
      A(K,K)=AK + 2.*CA*A(J,K) + CA*CA*A(J,J)                           JAC00136
      B(K,K)=BK + 2.*CA*B(J,K) + CA*CA*B(J,J)                           JAC00137
      A(J,J)=A(J,J) + 2.*CG*A(J,K) + CG*CG*AK                           JAC00138
      B(J,J)=B(J,J) + 2.*CG*B(J,K) + CG*CG*BK                           JAC00139
      A(J,K)=0.                                                         JAC00140
      B(J,K)=0.                                                         JAC00141
C                                                                       JAC00142
C     UPDATE THE EIGENVECTOR MATRIX AFTER EACH ROTATION                 JAC00143
C                                                                       JAC00144
      DO 200 I=1,N                                                      JAC00145
      XJ=X(I,J)                                                         JAC00146
      XK=X(I,K)                                                         JAC00147
      X(I,J)=XJ + CG*XK                                                 JAC00148
  200 X(I,K)=XK + CA*XJ                                                 JAC00149
  210 CONTINUE                                                          JAC00150
C                                                                       JAC00151
C     UPDATE THE EIGENVALUES AFTER EACH SWEEP                           JAC00152
C                                                                       JAC00153
      DO 220 I=1,N                                                      JAC00154
      IF (A(I,I).GT.0. .AND. B(I,I).GT.0.) GO TO 220                    JAC00155
      WRITE (IOUT,2020)                                                 JAC00156
      GO TO 800                                                         JAC00157
  220 EIGV(I)=A(I,I)/B(I,I)                                             JAC00158
      IF (IFPR.EQ.0) GO TO 230                                          JAC00159
      WRITE (IOUT,2030)                                                 JAC00160
      WRITE (IOUT,2010) (EIGV(I),I=1,N)                                 JAC00161
C                                                                       JAC00162
C     CHECK FOR CONVERGENCE                                             JAC00163
C                                                                       JAC00164
  230 DO 240 I=1,N                                                      JAC00165
      TOL=RTOL*D(I)                                                     JAC00166
      DIF=ABS(EIGV(I)-D(I))                                             JAC00167
      IF (DIF.GT.TOL) GO TO 280                                         JAC00168
  240 CONTINUE                                                          JAC00169
C                                                                       JAC00170
C     CHECK OFF-DIAGONAL ELEMENTS TO SEE IF ANOTHER SWEEP IS NEEDED     JAC00171
C                                                                       JAC00172
      EPS=RTOL**2                                                       JAC00173
      DO 250 J=1,NR                                                     JAC00174
      JJ=J + 1                                                          JAC00175
      DO 250 K=JJ,N                                                     JAC00176
      EPSA=(A(J,K)/A(J,J))*(A(J,K)/A(K,K))                              JAC00177
      EPSB=(B(J,K)/B(J,J))*(B(J,K)/B(K,K))                              JAC00178
      IF (EPSA.LT.EPS .AND. EPSB.LT.EPS) GO TO 250                      JAC00179
      GO TO 280                                                         JAC00180
  250 CONTINUE                                                          JAC00181
C                                                                       JAC00182
C     FILL OUT BOTTOM TRIANGLE OF RESULTANT MATRICES, SCALE EIGENVECTORSJAC00183
C                                                                       JAC00184
  255 DO 260 I=1,N                                                      JAC00185
      DO 260 J=I,N                                                      JAC00186
      A(J,I)=A(I,J)                                                     JAC00187
  260 B(J,I)=B(I,J)                                                     JAC00188
      DO 270 J=1,N                                                      JAC00189
      BB=SQRT(B(J,J))                                                   JAC00190
      DO 270 K=1,N                                                      JAC00191
  270 X(K,J)=X(K,J)/BB                                                  JAC00192
      GO TO 900                                                         JAC00193
C                                                                       JAC00194
C     UPDATE  D  MATRIX AND START NEW SWEEP, IF ALLOWED                 JAC00195
C                                                                       JAC00196
  280 DO 290 I=1,N                                                      JAC00197
  290 D(I)=EIGV(I)                                                      JAC00198
      IF (NSWEEP.LT.NSMAX) GO TO 40                                     JAC00199
      GO TO 255                                                         JAC00200
C                                                                       JAC00201
  800 STOP                                                              JAC00202
  900 RETURN                                                            JAC00203
C                                                                       JAC00204
 2000 FORMAT (//,' SWEEP NUMBER IN *JACOBI* = ',I8)                     JAC00205
 2010 FORMAT (' ',6E20.12)                                              JAC00206
 2020 FORMAT (//,' *** ERROR *** SOLUTION STOP',/,                      JAC00207
     1        ' MATRICES NOT POSITIVE DEFINITE')                        JAC00208
 2030 FORMAT (//,' CURRENT EIGENVALUES IN *JACOBI* ARE',/)              JAC00209
      END                                                               JAC00210
