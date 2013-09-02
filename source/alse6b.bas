   KEY OFF
   DEFINT I:DEFDBL C
   EXT$=".RTE"

   REM Set to 1 to Debug the database lookup routine
   DBG = 0

   REM Declare "fast" retrieval key arrays and Open the databases
   IMAXFILES=3:FFILE%=2
   DIM NREC(IMAXFILES+1),FN$(IMAXFILES+2)
   DIM KEYSIZE(IMAXFILES+1),RECSIZE(IMAXFILES+2)

   REM ALLOCATE OUR OWN KEY STORAGE AREA WITHIN INDIVIDUAL 64K SEGMENTS
   MAXKYOFF#=65535.
   IMAXKEYS=(MAXKYOFF#/4.)-1.
   DIM KYA2#(IMAXKEYS)
   DIM KYA3#(IMAXKEYS)
   DIM KYA4#(IMAXKEYS)

   REM Database Query variables
   RFILES%=3         :REM Files to query (Default is Airports)
   IMAXQUERY = 1001  :REM Maximum # of returned result matches
   IMAXRESULTS=6     :REM Maximum # of returned fields (Columns)
   DIM QRESULT#(IMAXQUERY,IMAXRESULTS)

   REM Results SORT field
   DIM SF$(IMAXRESULTS),SF%(IMAXRESULTS)
   SF$(0)="ID":SF%(0)=0

   REM Things that can be returned by setting a non zero column position
   REM  Note, column position may be changed by the calling routine
   REM  If the value is zero, the query calculation is NOT to be run
   REM The ID of the point is always returned in column 0
   REM   QF         FILE% of data base matched
   REM   QLEG       Distance "TO" the point
   REM   QTDS       Total distance
   REM   QDELTA     Delta difference in distance
   REM   QFUEL      Fuel Price
   REM   QCOST      Extended cost
   REM   QROUTE     Suggested optimal ordering of waypoints

   REM   QSTATE     Not returned in the array, but causes STATE to be displayed
   REM   QCITY      Not returned in the array, but causes CITY to be displayed

   REM Pack a 1 to 5 character key into a 32 bit integer
   DEF FNPKEY#( KY$ )

      LKY#=0
      FOR IK = 1 TO LEN(KY$)
        LKY# = (64*LKY#) + ASC(MID$(KY$,IK,1))-32
      NEXT IK

      FNPKEY# = LKY#
   END DEF

   REM Expand the key string from a packed key value
   DEF FNEKEY$( KY# )

      LKY# = KY#
      FKY$=""
      WHILE LKY# > 0
         C = (LKY# AND 63)
         LKY# = (LKY# - C) / 64
         FKY$ = CHR$(32 + C) + FKY$
      WEND

      FNEKEY$ = FKY$

   END DEF

   REM ***** Comment out this code to save program memory
   REM REM Pack string key into selected key array element
   REM FUNCTION SKEY#(A#(),REC%, KY$) STATIC

      REM A#(REC%) = FNPKEY#(KY$)
      REM SKEY# = A#(REC%)

   REM END FUNCTION

   REM REM Expand the key string selected from a key array
      REM FUNCTION GKEY$(A#(),REC%) STATIC

      REM   GKEY$ = FNEKEY$(A#(REC%))

      REM END FUNCTION

   REM GOTO 50
   REM FOR I = FFILE% TO IMAXFILES+1
   REM FOR II = 0 TO 5
   REM    KY$="F"+CHR$(48+I)+CHR$(II+65)
   REM    I# = FNPKEY#(KY$)
   REM    IF I = 2 THEN KYA2#(II) = I#
   REM    IF I = 3 THEN KYA3#(II) = I#
   REM    IF I = 4 THEN KYA4#(II) = I#
   REM    PRINT "Setting ";i;ii,ky$;i#
   REM NEXT II
   REM NEXT I

   REM II = 1
   REM WHILE II <> 0
   REM     INPUT "Test value of what key pair (file,record)";II,i

   REM     IF II = 2 THEN KY$ = FNEKEY$(KYA2#(I))
   REM     IF II = 3 THEN KY$ = FNEKEY$(KYA3#(I))
   REM     IF II = 4 THEN KY$ = FNEKEY$(KYA4#(I))
   REM     IF II = 2 THEN KYG$ = GKEY$(KYA2#(),I)
   REM     IF II = 3 THEN KYG$ = GKEY$(KYA3#(),I)
   REM     IF II = 4 THEN KYG$ = GKEY$(KYA4#(),I)
   REM     PRINT "Value for file";II;"record";i;"is ";ky$,KYG$
   REM WEND
   REM INPUT "Press enter to continue";O$
REM 50 REM

   GOSUB CLRKEYS
   GOSUB 1000

   PI=3.1415926536#:DG = 180/PI: REM  PI and Degree conversion constants
   FAACYCLE$=""

   REM Printer defaults
   DFPRTR$="LPT1:"
   PRINIT$=CHR$(15)+CHR$(27)+CHR$(48)+CHR$(13)
   PRSUPER$=CHR$(27)+"S0"
   SUB$=CHR$(27)+"S1"
   NORMAL$=CHR$(27)+"T"

   REM Degree symbol for the Screen and Printer
   DIM DEGSYM$(2)
   DEGSYM$(0)="ø"  :REM Screen
   DEGSYM$(1)="ø"  :REM Printer

   REM Defaults for airplane performance
   CLIMBSP=.75:FPM=500:CALT=9000:KTS=158
   FUELLOAD=90      :REM Departure fuel load (Gal)
   FUELCAP=90       :REM Max fuel capacity
   FUELMIN=13       :REM Minimum confortable fuel
   FUELBURN=13.3    :REM Cruise fuel burn rate  (Gal/hr)
   HOURLY= 10       :REM Allowance for consumable hourly expenses
   RUNWYHD=170      :REM No wind runway heading

       REM Allowable increase in direct routing distance
   QINCREASE = 1.10   :REM 10% increase
   QROUTEWIDTH = 60   :REM OR within nm, which ever is greater

   REM configuration defaults
   CONFIG$="ALSE6B.CFG"
   PROFILE$="ALSE6B.DEF"

   REM read the printer configuration file
   ON ERROR GOTO 65
   OPEN "I",#1,CONFIG$

   INPUT #1,I$:IF I$<> "" THEN PROFILE$=I$

   INPUT #1,I$:IF I$<> "" THEN DFPRTR$=I$

   INPUT #1,II:PRINIT$=""
   FOR I=1 TO II:INPUT#1,III:PRINIT$=PRINIT$+CHR$(III):NEXT I

   INPUT #1,II:DEGSYM$(1) = ""
   FOR I=1 TO II:INPUT#1,III:DEGSYM$(1)=DEGSYM$(1)+CHR$(III):NEXT I

   INPUT #1,II:PRSUPER$ = ""
   FOR I=1 TO II:INPUT#1,III:PRSUPER$=PRSUPER$+CHR$(III):NEXT I

   INPUT #1,II:SUB$ = ""
   FOR I=1 TO II:INPUT#1,III:SUB$=SUB$+CHR$(III):NEXT I

   INPUT #1,II:NORMAL$ = ""
   FOR I=1 TO II:INPUT#1,III:NORMAL$=NORMAL$+CHR$(III):NEXT I

   CLOSE #1
   FONE%=0
   GOTO 69
65 RESUME 69
69 ON ERROR GOTO 0

   REM get user defaults for airplane performance
   GOSUB 80
   IF STATUS$="OK" THEN GOSUB 85:CLOSE #1
   GOTO 90

80 REM Open the performance profile
   ON ERROR GOTO 82
   STATUS$="OK"
   OPEN "I",#1,PROFILE$
   GOTO 84
82 STATUS$="NO FILE"
   RESUME 84
84 ON ERROR GOTO 0
   RETURN

85 REM Read base aircraft performance specification
   INPUT #1,KTS,CLIMBSP,FPM,CALT
   INPUT #1,QINCREASE,QROUTEWIDTH
   INPUT #1,FUELLOAD,FUELCAP,FUELMIN,FUELBURN,HOURLY
   INPUT #1,RUNWYHD
   RETURN

90 AVGGS=KTS
   WAYPT=0
   MAXPTS = 40
   DIM PL$(MAXPTS),IDENT$(MAXPTS),FREQ$(MAXPTS),MV(MAXPTS)
   DIM TC(MAXPTS),DS(MAXPTS),ELEV(MAXPTS)
   DIM LADEG(MAXPTS),LODEG(MAXPTS),LAMIN(MAXPTS),LOMIN(MAXPTS)
   DIM TYPE$(MAXPTS),CTY$(MAXPTS),ST$(MAXPTS)
   DIM AIRSP(MAXPTS),WS(MAXPTS),WD(MAXPTS)

   REM Used during fuel cost swags
   DIM FUELP(MAXPTS),FUELARRIVE(MAXPTS),FUELADD(MAXPTS),FUELUSEABLE(MAXPTS)

   REM Used for Route queries
   DIM QRTEWPT%(MAXPTS)
   QENDPT%=0: REM Route Begin/End point
   QSFTPT%=1: REM Suggested intermediate point, not required to be flown
   QHRDPT%=2: REM Required intermediate waypoint, must be flown

   DIM QRECT!(1,1)

   GOTO 40000:  Rem Jump to Main Menu

CLRKEYS: PRINT"clearing retrieval keys..."
         FOR III=1 TO IMAXFILES
           FOR II=0 TO IMAXKEYS-1
             IF I = 2 THEN KYA2#(II) = 0
             IF I = 3 THEN KYA3#(II) = 0
             IF I = 4 THEN KYA4#(II) = 0
           NEXT II
         NEXT III
         RETURN

SORT: REM First, read the file to get the keys
      IF NREC(FILE%) < 2 THEN RETURN
      PRINT "Indexing";NREC(FILE%);"records... "
      FIELD #FILE%,KEYSIZE(FILE%) AS ID$, RECSIZE(FILE%)-KEYSIZE(FILE%) AS DATA$
      RESORT = 0

      REM  We need to allocate another array, so free up memory not needed.
      REM  This is okay as we will re-initial everything when completed.
      IF FILE% <> 2 THEN ERASE KYA2#
      IF FILE% <> 3 THEN ERASE KYA3#
      IF FILE% <> 4 THEN ERASE KYA4#
      DIM NEWPOS#(IMAXKEYS)

      FOR I = 1 TO NREC(FILE%)
         GET #FILE%,I
         NEWPOS#(I) = I
         IF FILE% = 2 THEN KYA2#(I) = FNPKEY#(ID$)
         IF FILE% = 3 THEN KYA3#(I) = FNPKEY#(ID$)
         IF FILE% = 4 THEN KYA4#(I) = FNPKEY#(ID$)
      NEXT I

      REM Now sort ascending by taking each key and finding the smallest
      REM   key in the file from that point foward and swapping it.
      FIELD #FILE%,RECSIZE(FILE%) AS DATA$
      J = NREC(FILE%) - 1
      WHILE J >= 1
         IF INT(J/750)*750 = J THEN PRINT CHR$(13);"Records left ";J;"/";NREC(FILE%);

         IF FILE% = 2 THEN PKY# = KYA2#(J+1):KY# = KYA2#(J)
         IF FILE% = 3 THEN PKY# = KYA3#(J+1):KY# = KYA3#(J)
         IF FILE% = 4 THEN PKY# = KYA4#(J+1):KY# = KYA4#(J)

         REM If Key(J+1) < Key(J), we are out of ascending sort order
         IF PKY# < KY# THEN
            RESORT = 1

            REM Search forward to find the spot for this key
            II = NREC(FILE%)
            I = J + 1
            WHILE I <= NREC(FILE%)

               IF FILE% = 2 THEN PKY# = KYA2#(I)
               IF FILE% = 3 THEN PKY# = KYA3#(I)
               IF FILE% = 4 THEN PKY# = KYA4#(I)

               REM As the bottom end of the list is already sorted in
               REM ascending, we can stop searching as soon as we've
               REM gone past the correct slot
               IF KY# < PKY# THEN II = I-1:I = NREC(FILE%)

               I = I + 1
            WEND

            REM Save the record at J
            RECNUM = NEWPOS#(J)

            REM Ripple move all Records from J+1 to I toward front of the file
            FOR I = J+1 TO II
               NEWPOS#(I-1) = NEWPOS#(I)

               IF FILE% = 2 THEN KYA2#(I-1) = KYA2#(I)
               IF FILE% = 3 THEN KYA3#(I-1) = KYA3#(I)
               IF FILE% = 4 THEN KYA4#(I-1) = KYA4#(I)

            NEXT I

            REM Write the saved record (J) into correct position (II)
            NEWPOS#(II) = RECNUM
            IF FILE% = 2 THEN KYA2#(II) = KY#
            IF FILE% = 3 THEN KYA3#(II) = KY#
            IF FILE% = 4 THEN KYA4#(II) = KY#

         ENDIF
         J = J -1

      WEND

      REM If the file order has been changed
      IF RESORT > 0 THEN

         REM We will copy the input to minimize the amount record by record
         REM  copying we will have to do.  This assumes that a bunch of the
         REM  file has not changed, so we will get some benefit to the copy.

         PRINT:PRINT "Creating temporary work file"
         CLOSE #FILE%
         SHELL "COPY "+FN$(FILE%)+DEXT$+" "+FN$(FILE%)+".$TP"

         REM Use a temporary file to reorder the records
         OPEN "R",#1,FN$(FILE%)+".$TP",RECSIZE(FILE%)
         FIELD #1,RECSIZE(FILE%) AS RECORD$
         IF LOF(1)/RECSIZE(FILE%) <> NREC(FILE%) THEN
            CLOSE
            PRINT
            PRINT "Internal error with temporary file: ";FN$(FILE%)+".$TP"
            PRINT "Check the amount of available disk space."
            PRINT FN$(FILE%)+DEXT$+" is not indexed properly."
            PRINT "Correct what ever is causing the above error and then Work"
            PRINT "with that Data type to rename an ID, thus causing a  reindexing"
            PRINT "of the data to take place."
            PRINT "Press ENTER to acknowledge...";I$
            RETURN
         ENDIF

         OPEN "R",#FILE%,FN$(FILE%)+DEXT$,RECSIZE(FILE%)
         FIELD #FILE%,RECSIZE(FILE%) AS DATA$

         REM Read sorted keys and fetch the original record in sorted order
         FOR I = 1 TO NREC(FILE%)

             RECNUM = NEWPOS#(I)
             IF RECNUM <> I THEN

                GET #FILE%,RECNUM
                LSET RECORD$=DATA$
                PUT #1,I
             ENDIF
             IF INT(I/350)*350 = I THEN PRINT CHR$(13);"Phase 2 ordering ";I;"/";NREC(FILE%);
         NEXT I

         REM Now copy the sorted results back from the Temporary file
         PRINT:PRINT "Renaming temporary file to permanent file"
         CLOSE
         KILL FN$(FILE%)+DEXT$
         SHELL "RENAME "+FN$(FILE%)+".$TP "+FN$(FILE%)+DEXT$

      ENDIF

      ERASE NEWPOS#
      RETURN

FMTCOORD:
      I$=STR$(ASC(LADEG$)-90)+"."+STR$(ASC(LAMIN$))+"."+STR$(ASC(LASEC$))
      GOSUB PARSELAT
      I$=STR$(ABS(ILODEG-180))+"."+STR$(ILOMIN)+"."+STR$(ILOSEC)
      IF ILODEG<180 THEN I$="E"+I$
      GOSUB PARSELONG
      RETURN

PARSECOORD:
      REM Parse the input Latitude format into our normal short hand notation
      I$=LAT$
      GOSUB PARSELAT

      REM Parse the input Longitude format into our normal short hand notation
      I$=LONG$
      GOSUB PARSELONG
      RETURN

INPUTLAT: PRINT"Enter 'degrees-minutes-seconds'"
          INPUT"Latitude (DD-MM-DM)";I$
          IF I$ = "" THEN RETURN

PARSELAT: REM Parse and Check Latitude
          III=0:II=1:LAT$="":MJ=90
          II$=I$+" "
PLAT1:    IF LEFT$(II$,1) = " " THEN II$=MID$(II$,2):GOTO PLAT1
          IF LEFT$(II$,1) = "N" OR LEFT$(II$,1) = "n" THEN II=2
          IF LEFT$(II$,1) = "S" OR LEFT$(II$,1) = "s" THEN II=2
          FOR I=II TO LEN(II$)
             IF MID$(II$,I,1) <= "9" AND MID$(II$,I,1) >= "0" OR I = II THEN GOTO PLAT2
             J=ABS(VAL(MID$(II$,II,I-II)))
             IF J > MJ THEN III=1000
             LAT$=LAT$+RIGHT$(STR$(100+J),2)+"-"
             III=III+1:II=I+1:MJ=59
PLAT2:    NEXT I
          LAT$=LEFT$(LAT$,LEN(LAT$)-1)
          IF LEN(LAT$) <> 8 OR III <> 3 THEN PRINT"Entry syntax error for ";I$:GOTO INPUTLAT
          IF LEFT$(II$,1) = "S" OR LEFT$(II$,1) = "s" OR LEFT$(II$,1) = "-" THEN LAT$="S"+LAT$ ELSE LAT$="N"+LAT$
          RETURN

INPUTLONG: PRINT"Enter 'degrees-minutes-seconds'"
           INPUT"Longitude (DD-MM-DM)";I$
           IF I$ = "" THEN RETURN

PARSELONG: REM Parse and Check Longitude
           III=0:II=1:LONG$="":MJ=180
           II$=I$+" "
PLONG1:    IF LEFT$(II$,1) = " " THEN II$=MID$(II$,2):GOTO PLONG1
           IF LEFT$(II$,1) = " " THEN II$=MID$(II$,2)
           IF LEFT$(II$,1) = "W" OR LEFT$(II$,1) = "w" THEN II=2
           IF LEFT$(II$,1) = "E" OR LEFT$(II$,1) = "e" THEN II=2:II$="-"+MID$(II$,2)
           FOR I=II TO LEN(II$)
              IF MID$(II$,I,1) <= "9"  AND MID$(II$,I,1) >= "0" OR I = II THEN GOTO PLONG2
              J=VAL(MID$(II$,II,I-II))
              IF J > MJ THEN III=1000
              IF III=0 THEN LONG$=RIGHT$("   "+STR$(J),3) ELSE LONG$=LONG$+RIGHT$(STR$(100+J),2)
              LONG$=LONG$+"-"
              III=III+1:II=I+1:MJ=59
PLONG2:   NEXT I
          LONG$=LEFT$(LONG$,LEN(LONG$)-1)
          IF LEN(LONG$) <> 9 OR III <> 3 THEN PRINT"Entry syntax error for ";i$:GOTO INPUTLONG
          IF LEFT$(II$,1) = "-" THEN LONG$="E"+LONG$ ELSE LONG$="W"+LONG$
          IF MID$(LONG$,2,1) = " " THEN LONG$=" "+LEFT$(LONG$,1)+MID$(LONG$,3)
          RETURN

SETLATLONG: REM Set Latitude and Longitude into a database record
            IF LEFT$(LAT$,1) = "S" OR LEFT$(LAT$,1)="s" THEN LAT$="-"+MID$(LAT$,2)
            IF LEFT$(LAT$,1) = "N" OR LEFT$(LAT$,1)="n" THEN LAT$=" "+MID$(LAT$,2)
            LSET LADEG$=CHR$(VAL(MID$(LAT$,1,3))+90)
            LSET LAMIN$=CHR$(VAL(MID$(LAT$,5,2)))
            LSET LASEC$=CHR$(VAL(MID$(LAT$,8,2)))

            IF LEFT$(LONG$,1) = " " THEN LONG$=MID$(LONG$,2,1) + " " + MID$(LONG$,3)
            IF LEFT$(LONG$,1) = "E" OR LEFT$(LONG$,1)="e" THEN LONG$="-"+MID$(LONG$,2)
            IF LEFT$(LONG$,1) = "W" OR LEFT$(LONG$,1)="w" THEN LONG$=" "+MID$(LONG$,2)
            ILODEG=VAL(MID$(LONG$,1,4))+180
            ILOMIN=VAL(MID$(LONG$,6,2))
            ILOSEC=VAL(MID$(LONG$,9,2))
            LSET LODEG$=CHR$(ILODEG - 256*INT(ILODEG/256))
            LSET LOMIN$=CHR$(ILOMIN)
            LSET LOSEC$=CHR$( (ILOSEC*2) + INT(ILODEG/256))
            RETURN

UNPKFREQ: REM Unpack the frequency as stored in the file
          REM Note, this intentionally falls into FMTFREQ
      J!=CVL(FREQ$)/1000!+125!

FMTFREQ: REM Format a frequency for standardize aviation output
      REM   Input is J!, Output is I$
      IF J! < 10! THEN I$=STR$(J!*100): RETURN
      J!=INT((J!+.0005)*1000)
      IF J! = 0 then I$="":RETURN
      IF J! > 150*1000! THEN I$=STR$(J!/100000): RETURN
      I$=STR$(J!)
      I$=LEFT$(I$,LEN(I$)-3)+"."+RIGHT$(I$,3)
      I$=MID$(I$,2)
      IF LEFT$(I$,1)="0" THEN I$=LEFT$(I$,LEN(I$)-1)+" "
      IF RIGHT$(I$,1)="0" THEN I$=LEFT$(I$,LEN(I$)-1)+" "
      RETURN

INPUTMAGV: REM Accept a new Magnetic Variation into VAR$
           INPUT"Magnetic variation";I$
           IF I$="" THEN RETURN
PARSEMAGV: IF LEFT$(I$,1)="E" OR LEFT$(I$,1)="e" OR LEFT$(I$,1)="W" OR LEFT$(I$,2)="w" THEN I = 2 ELSE I = 1
           IF LEFT$(I$,1)="?" THEN VAR$="?":RETURN
           I=VAL(MID$(I$,I))
           IF I <= -128 THEN VAR$="?":RETURN
           IF LEFT$(I$,1)="E" OR RIGHT$(I$,1)="E" OR LEFT$(I$,1)="e" OR RIGHT$(I$,1)="e" THEN I =-1*I
           VAR$=STR$(ABS(I))
           IF I < 0 THEN VAR$=VAR$+"E"
           IF I > 0 THEN VAR$=VAR$+"W"
           RETURN

SETMAGV: REM Set Magnetic Variation into a data record
         I$=VAR$
         IF LEFT$(I$,1)="?" OR VAR$ = "" THEN LSET DEV$=CHR$(0):RETURN
         IF LEFT$(I$,1)="E" OR LEFT$(I$,1)="e" OR LEFT$(I$,1)="W" OR LEFT$(I$,2)="w" THEN II = 2 ELSE II = 1
         II=VAL(MID$(I$,II))
         IF LEFT$(I$,1)="E" OR RIGHT$(I$,1)="E" OR LEFT$(I$,1)="e" OR RIGHT$(I$,1)="e" THEN II =-1*II
         LSET DEV$=CHR$(II+128)
         RETURN

            REM Expand Revision date
FMTREVDATE: II$ = MID$(STR$(CVL(I$)),2)
            II$ = MID$(II$,5,2)+"/"+MID$(II$,7,2)+"/"+LEFT$(II$,4)
            RETURN

             REM Compact Revision date mm/dd/yyyy into Sortable long binary
PACKREVDATE: I = INSTR(1,I$,"/")
             II = INSTR(I+1,I$,"/")
             J& = VAL(MID$(I$+"    ",II+1,4))              :REM Year
             IF J& < 100 THEN J&=1900+J&
             J& = (J& * 100!) + VAL(LEFT$(I$,I-1))         :REM Month
             J& = (J& * 100!) + VAL(MID$(I$,I+1,II-I-1))   :REM Day
             II$ = MKL$(J&)
             RETURN

PUTNAV: REM Put a Navaid record into the database at RECNUM
        GOSUB SETLATLONG
        LSET CITY$=CTY$
        LSET STATE$=ST$
        LSET ELEV$=MKI$(VAL(ELE$))
        LSET APT$=NAM$
        LSET FREQ$=MKL$((VAL(TWR$)-125)*1000)
        GOSUB SETMAGV:  REM Set Magnetic Variation into record buffer

        I$=CYCLE$:GOSUB PACKREVDATE:LSET REVDATE$ = II$

        PUT #FILE%,RECNUM
        RETURN

PUTFIX: REM Put a Fix record into the database at RECNUM
        GOSUB SETLATLONG
        LSET STATE$=ST$
        LSET APT$=NAM$
        GOSUB SETMAGV:  REM Set Magnetic Variation into record buffer
        LSET REFID$=REF1$
        LSET RADIAL$=MKI$(RADIAL)
        LSET DIST$=MKI$(10*DIST)

        I$=CYCLE$:GOSUB PACKREVDATE:LSET REVDATE$ = II$

        PUT #FILE%,RECNUM
        RETURN

PUTAPT: REM Put an Airport record into the database at RECNUM
        GOSUB SETLATLONG
        LSET CITY$=CTY$
        LSET STATE$=ST$
        LSET ELEV$=MKI$(VAL(ELE$))
        LSET APT$=NAM$
        LSET FREQ$=MKL$((VAL(TWR$)-125)*1000)
        GOSUB SETMAGV:  REM Set Magnetic Variation into record buffer
        LSET FUEL$=MKI$(VAL(FBO$)*1000)

        I$=CYCLE$:GOSUB PACKREVDATE:LSET REVDATE$ = II$

        IF DTLCHANGE$ = "TRUE" THEN GOSUB PUTAPTDTL

        PUT #FILE%,RECNUM
        RETURN

PUTAPTDTL: REM Update AIRPORT Details (Non indexed file)
           IF ATIS$="" AND RNWY$ = "" AND FBONAME$ = "" AND TFREQS$ = "" AND GROUND$="" AND APPROACH$="" THEN RETURN
           AINDX& = CVL(APTINFO$)

           IF AINDX& > 0 THEN ADDING=0:GOTO APTDTL2

           REM Adding new information at the end of the file
           ADDING = 1
           IF LOF(APTDTL%) > 0 THEN GOTO APTDTL1
           II$="xxxxxxxx"+CHR$(13)+CHR$(10)
           II$=II$+"Airport Details"+CHR$(13)+CHR$(10)
           II$=II$+" ID,Frequencies,ATIS,Ground,Approach,Runways, FBO"+CHR$(13)+CHR$(10)
           AINDX& = LEN(II$)
           II$=LEFT$(STR$(AINDX&)+"        ",8)+MID$(II$,9)
           LSET DB$=II$
           PUT #APTDTL%,1

           REM Update the APTINFO$ variable with where we are saving these details
APTDTL1:   GET #APTDTL%,1
           AINDX& = VAL(DB$)
           LSET APTINFO$=MKL$(AINDX&)

APTDTL2:   III$=CHR$(34)+","+CHR$(34)
           WHILE RIGHT$(FBONAME$,1) = " "
              FBONAME$=LEFT$(FBONAME$,LEN(FBONAME$)-1)
           WEND
           II$=CHR$(34)+ID$+III$+TFREQS$+III$+ATIS$+III$+GROUND$+III$+APPROACH$+III$
           II$=II$+RNWY$+III$+FBONAME$+III$+CHR$(34)

           REM Get the initial buffer that will start the record
           I = INT(AINDX&/RECSIZE(APTDTL%)):II = AINDX& - (I * RECSIZE(APTDTL%)) + 1
           GET #APTDTL%,I+1
           III$=DB$

           REM Is this an ADD of new data, or update of existing details
           IF ADDING = 1 THEN GOTO APTDTL3

           REM Get all the old record
           J = 1
           WHILE INSTR(II,III$,CHR$(13)) = 0
              J = J + 1
              GET #APTDTL%,I+J
              III$=III$+DB$
           WEND

           REM compute where the old data resides and If we can update inplace
           J = INSTR(II,III$,CHR$(13))
           IF LEN(II$) > (J - II) THEN ADDING = 1:GOTO APTDTL3

           REM Pad out to the size of the existing hole
           WHILE LEN(II$) < (J - II)
              II$ = LEFT$(II$,LEN(II$)-1) + "*" + RIGHT$(II$,1)
           WEND
           GOTO APTDTL4

           REM Adding to the end, must update our free space pointer in 1st record
APTDTL3:   II$=II$ + CHR$(13)+CHR$(10)
           GET #APTDTL%,1
           AINDX&=VAL(DB$)
           LSET DB$=LEFT$(STR$(AINDX&+LEN(II$))+"        ",8)+MID$(DB$,9)
           PUT #APTDTL%,1
           IF I = 0 THEN III$=DB$+MID$(III$,RECSIZE(APTDTL%)+1)

APTDTL4:   REM
           IF II > 1 THEN III$=LEFT$(III$,II-1)+II$+MID$(III$,II+LEN(II$)) ELSE III$ = II$+MID$(III$,LEN(II$)+1)
           LSET DB$=LEFT$(III$,RECSIZE(APTDTL%))
           PUT #APTDTL%,I+1

           II = 1
           WHILE LEN(III$) > (II*RECSIZE(APTDTL%))
              LSET DB$=MID$(III$,(II*RECSIZE(APTDTL%))+1)
              II = II + 1
              PUT #APTDTL%,I+II
           WEND

           RETURN

SORTNAV: REM Sort the Navaids
         FILE%=2
         GOTO SORTANY

SORTAPT: REM Sort the Airports
         FILE%=3
         GOTO SORTANY

SORTFIX: REM Sort the Fixes
         FILE%=4
         REM Fall into the SORTANY subroutine

         REM Close all the files, re-open, and force getting new keys
SORTANY: GOSUB 1000
         GOSUB SORT
         CLOSE

         REM Now cause everything to be re-initialized
         RUN

100 REM Search all the databases
    IF INSTR(1,I$," ") > 0 THEN I$=LEFT$(I$,INSTR(1,I$," ")-1)
    III$=I$
    FILE%=4  :  REM         Search FIXES database
    IF LEN(I$)<> KEYSIZE(FILE%) THEN 120

    GOSUB 400
    IF II$<>ID$ THEN I$ = III$:GOTO 120

    REM We found the fix, but we would like to have the VOR reference frequency
    I$=REFID$:FILE%=2:GOSUB 400:FUZZY$=FREQ$(WAYPT)

    I$=III$:FILE%=4:GOSUB 400:FREQ$(WAYPT)=FUZZY$

    PL$(WAYPT)=REFID$+" @r"+MID$(STR$(CVI(RADIAL$)),2)+"/"+MID$(STR$(CVI(DIST$)/10),2)+"nm"
    CTY$(WAYPT)=PL$(WAYPT)
    RETURN

120 FILE%=2  :  REM         Search VOR/NDB database
    IF LEN(I$) > KEYSIZE(FILE%) THEN 130

    GOSUB 400
    IF II$=ID$ THEN PL$(WAYPT)=CITY$:RETURN
    I$ = III$

130 REM         Prepare to search the AIRPORT database
    FILE%=3
    IF LEN(I$) > KEYSIZE(FILE%) THEN 170
    IF LEN(I$) > 3 THEN GOSUB 190
    REM         SEARCH AIRPORT DATABASE
    GOSUB 400

    IF II$=ID$ THEN PL$(WAYPT)=APT$:IDENT$(WAYPT)=III$

170 REM         All appropriate databases have been searched
    RETURN

190 REM Handle stripping of special airport prefix
    IF INSTR(1,"KPC",LEFT$(I$,1))=0 THEN RETURN
    FOR I=1 TO 4
        IF MID$(I$,I,1) < "A" THEN RETURN
    NEXT I
    I$=MID$(I$,2)
    RETURN

    REM SEARCH ANY DATABASE FILE FOR RECORD IDENTIFIED BY I$
    REM  Note, I$ is destroyed.  A match will return ID$=II$
400 I=1:II=NREC(FILE%)
    IF DBG > 0 THEN PRINT "Looking up ";I$,"There are ";II;" records in file";FILE%
    ON FILE%-1 GOSUB 1120,1130,1140
    II$=LEFT$(I$+"     ",KEYSIZE(FILE%))
    IF NREC(FILE%) = 0 THEN LSET ID$=MID$("~~~~~~~",1,KEYSIZE(FILE%)):GOTO 490
    PKY# = FNPKEY#(II$):IF DBG > 0 THEN PRINT "   Packed search key is";PKY#
430 III=INT((I+II)/2)
    IF FILE%=2 THEN KY# = KYA2#(III)
    IF FILE%=3 THEN KY# = KYA3#(III)
    IF FILE%=4 THEN KY# = KYA4#(III)
    IF DBG>0 THEN PRINT " KEY("III;") is ";FNEKEY$(KY#);KY#,"Range:";I;II
    IF KY# = 0 OR KY# = PKY# THEN 440
    IF PKY# > KY# THEN I=III+1 ELSE II = III
    IF I<II THEN 430
    III=I
440 GET #FILE%,III: REM PRINT I,II,III
    KY#=FNPKEY#(ID$)
    IF DBG > 0 THEN PRINT "Read record";III,"Key is ";ID$;"  packed:";KY#
    IF FILE%=2 THEN KYA2#(III) = KY#
    IF FILE%=3 THEN KYA3#(III) = KY#
    IF FILE%=4 THEN KYA4#(III) = KY#
    IF PKY# = KY# OR I >= II THEN 475
    IF PKY# > KY# THEN I=III+1 ELSE II=III
    GOTO 430
475 REM Found the correct record
    IDENT$(WAYPT)=ID$
    ELEV(WAYPT)=CVI(ELEV$)
    IF FILE%=4 THEN FREQ$(WAYPT)="" ELSE GOSUB UNPKFREQ:FREQ$(WAYPT)=I$
    MV(WAYPT)=ASC(DEV$)-128
    GOSUB 500
    TYPE$(WAYPT)=APT$
    CTY$(WAYPT)=CITY$
    ST$(WAYPT)=STATE$
    WD(WAYPT)=0:WS(WAYPT)=0

    FUELP(WAYPT) = 0:IF FILE%=3 THEN FUELP(WAYPT) = CVI(FUEL$)/1000

490 IF DBG>0 THEN PRINT "Returning '";ID$;"' for requested ";II$:INPUT"Continue Debug";YUP$
    RETURN

    REM Save a little bit of redundant code
499 WAYPT=WAYPT+1
    I$=II$
    GOSUB 400
    WAYPT=WAYPT-1
    RECNUM=III
    RECID$=ID$: REM save the original record ID to see if sort is needed
    RETURN

500 REM Convert/Unpack Latitude & Longitude from the database
    REM Latitude ranges +- 90 degrees
    LADEG(WAYPT)=ASC(LADEG$)-90
      LAMIN(WAYPT)=ASC(LAMIN$)+ASC(LASEC$)/60.

    REM As Longitude ranges +-180 degrees, the high order part is packed
    REM   in with the seconds
    ILOSEC=INT(ASC(LOSEC$)/2)
    ILODEG=ASC(LODEG$)+256*(ASC(LOSEC$)-(2*ILOSEC))
    ILOMIN=ASC(LOMIN$)
    LODEG(WAYPT)=ILODEG-180
      LOMIN(WAYPT)=ASC(LOMIN$)+INT(ASC(LOSEC$)/2)/60.
    RETURN


1000 REM open database files   (Note, this will alter FILE%)
     REM Assignments are:
     REM   #1 Miscellaneous INPUT/OUTPUT
     REM   #2 NAVAIDS
     REM   #3 AIRPORTS
     REM   #4 FIXES
     REM   #5 AIRPORT DETAILS
     OUTPUT$="SCRN:"
     DEXT$=".DAT"

     REM Don't destroy the existing FILE% value
     III = FILE%

     FILE%=2:RECSIZE(FILE%)=59
     FN$(FILE%)="NAVAIDS"
     OPEN "R",#FILE%,FN$(FILE%)+DEXT$,RECSIZE(FILE%)
     NREC(FILE%) = LOF(FILE%)/RECSIZE(FILE%)
     KEYSIZE(FILE%)=5
     GOSUB 1120

     FILE%=3:RECSIZE(FILE%)=84
     FN$(FILE%)="AIRPORTS"
     OPEN "R",#FILE%,FN$(FILE%)+DEXT$,RECSIZE(FILE%)
     NREC(FILE%) = LOF(FILE%)/RECSIZE(FILE%)
     KEYSIZE(FILE%)=4
     GOSUB 1130

     FILE%=4:RECSIZE(FILE%)=40
     FN$(FILE%)="FIXES"
     OPEN "R",#FILE%,FN$(FILE%)+DEXT$,RECSIZE(FILE%)
     NREC(FILE%) = LOF(FILE%)/RECSIZE(FILE%)
     KEYSIZE(FILE%)=5
     GOSUB 1140

     APTDTL%=5
     RECSIZE(APTDTL%) = 125
     FN$(APTDTL%)="AIRPORTS.DTL"
     OPEN "R",#APTDTL%,FN$(APTDTL%),RECSIZE(APTDTL%)
     FIELD #APTDTL%,RECSIZE(APTDTL%) AS DB$

     FILE% = III
     RETURN

1120 REM Set field definitions for Navaids
     FILE%=2
     FIELD #FILE%,KEYSIZE(FILE%) AS ID$,1 AS LADEG$,1 AS LAMIN$,1 AS LASEC$,1 AS LODEG$, 1 AS LOMIN$,1 AS LOSEC$,20 AS APT$,15 AS CITY$,2 AS STATE$, 4 AS REVDATE$,1 AS DEV$,2 AS ELEV$,4 AS FREQ$
     RETURN

1130 REM Set field definitions for Airports
     FILE%=3
     FIELD #FILE%,KEYSIZE(FILE%) AS ID$,1 AS LADEG$,1 AS LAMIN$,1 AS LASEC$,1 AS LODEG$, 1 AS LOMIN$,1 AS LOSEC$,35 AS APT$,20 AS CITY$,2 AS STATE$, 4 AS REVDATE$,1 AS DEV$,2 AS ELEV$,4 AS FREQ$, 2 AS FUEL$, 4 AS APTINFO$
     RETURN

1140 REM Set field definitions for Fixes
     FILE%=4
     FIELD #FILE%,KEYSIZE(FILE%) AS ID$,1 AS LADEG$,1 AS LAMIN$,1 AS LASEC$,1 AS LODEG$, 1 AS LOMIN$,1 AS LOSEC$,15 AS APT$,2 AS STATE$, 4 AS REVDATE$, 1 AS DEV$, 3 AS REFID$, 2 AS RADIAL$, 2 AS DIST$
     CITY$=""
     FREQ$=""
     RETURN

FMTNAV:REM Format a Navagational AID for display
      GOSUB UNPKFREQ:TWR$=I$
      I$=STR$(ASC(DEV$)-128):GOSUB PARSEMAGV:Rem format the VAR$ (Magnetic Variation)
      NAM$=APT$
      ST$=STATE$
      CTY$=CITY$
      ELE$=RIGHT$("      "+STR$(CVI(ELEV$)),6)

      I$ = REVDATE$:GOSUB FMTREVDATE:CYCLE$=II$:REM Format the Revision Cycle Date

      GOSUB FMTCOORD:

      RETURN

FMTAPT: REM Format an Airport for display
        GOSUB UNPKFREQ:TWR$=I$
        I$=STR$(ASC(DEV$)-128):GOSUB PARSEMAGV:Rem format the VAR$ (Magnetic Variation)
        NAM$=APT$
        ST$=STATE$
        CTY$=CITY$
        ELE$=RIGHT$("      "+STR$(CVI(ELEV$)),6)

        GOSUB FMTCOORD:

        FBO$ = STR$(CVI(FUEL$))
        IF LEN(FBO$) > 3 THEN FBO$=LEFT$(FBO$,LEN(FBO$)-3)+"."+RIGHT$(FBO$,3)
        IF CVI(FUEL$) = 0 THEN FBO$="" ELSE IF LEFT$(FBO$,1)=" " THEN FBO$=MID$(FBO$,2)
        IF RIGHT$(FBO$,1) = "0" THEN FBO$=LEFT$(FBO$,LEN(FBO$)-1)

        I$ = REVDATE$:GOSUB FMTREVDATE:CYCLE$=II$:REM Format the Revision Cycle Date

        GOSUB FMTAPTDTL
        RETURN

FMTAPTDTL: REM Handle the fields from the Airport Information Extension file
        ATIS$="":GROUND$="":APPROACH$="":RNWY$="":FBONAME$=""
        TFREQS$="":FUELINFO$=FBO$+FBONAME$
        AINDX& = CVL(APTINFO$)
        IF AINDX& < 1 THEN RETURN

        REM Look up the extended airport information in the extension file

        REM Get the buffers that the record spans
        I = INT(AINDX&/RECSIZE(APTDTL%)):II = AINDX& - (I * RECSIZE(APTDTL%))

        GET #APTDTL%,I+1:II$=DB$
        IF II > 0 THEN II$=MID$(II$,II+1)

        REM Read all of the record into the string buffer
        II = 1
        WHILE INSTR(1,II$,CHR$(13)) = 0
          II = II + 1
          GET #APTDTL%, I+II
          II$=II$+DB$
        WEND

        REM Sanity check the extra detail record
        IF MID$(II$,2,LEN(ID$)) <> ID$ THEN RETURN

        REM Extract the fields from the buffers
        III$=CHR$(34)+","+CHR$(34)
        I = INSTR(1,II$,III$)+LEN(III$)
        II = INSTR(I,II$,III$)
        TFREQS$ = MID$(II$,I,II-I)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        ATIS$ = MID$(II$,1,II-1)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        GROUND$ = MID$(II$,1,II-1)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        APPROACH$ = MID$(II$,1,II-1)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        RNWY$ = MID$(II$,1,II-1)

        II$= MID$(II$,II+LEN(III$))
        II = INSTR(1,II$,III$)
        FBONAME$ = MID$(II$,1,II-1)

        IF FBO$ = "" THEN FUELINFO$=FBONAME$ ELSE FUELINFO$=FBO$+" "+FBONAME$

        RETURN

FMTFIX: REM Format a Navigational FIX for displaying
        I$=STR$(ASC(DEV$)-128):GOSUB PARSEMAGV:Rem format the VAR$ (Magnetic Variation)
        NAM$=APT$
        ST$=STATE$
        CTY$=CITY$

        GOSUB FMTCOORD:
        REF1$=REFID$
        RADIAL=CVI(RADIAL$)
        DIST=CVI(DIST$)/10

        I$ = REVDATE$:GOSUB FMTREVDATE:CYCLE$=II$:REM Format the Revision Cycle Date

        RETURN

REM *************************************************************************
REM   EVERYTHING BEFORE THIS POINT IS IN COMMON WITH ALSE6B & ALSE6BCF
REM   CHANGES SHOULD BE DUPLICATED IN BOTH MODULES (Or Block Copied)
REM *************************************************************************

2000 REM Enter a new route
     ALL=0
     WAYPT=0
2005   REM Entry point for EDIT an existing route
     LL=3
     CLS
     PRINT
     PRINT "   ID    NAME               WHERE         LAT        LONG      FREQ    MAGVAR"
     IF ALL=0 THEN 2100
     PNT=ALL
     WAYPT=0
     FOR OLD=0 TO PNT-1
       I$=IDENT$(OLD)
       GOSUB 100
       GOSUB 2240
     NEXT
     ALL=PNT

2100 LOCATE LL,1
     I$=""
     IF WAYPT=ALL THEN PRINT SPC(79) ELSE I$=IDENT$(WAYPT)
2105 GOSUB 3020
     IF II$=CHR$(27) OR I$="" THEN RETURN
     I$=LEFT$(I$,5)
     FOR JJ=1 TO LEN(I$)
       IF LEFT$(I$,1) > "Z" THEN I$=MID$(I$,2)+CHR$(ASC(I$)-32) ELSE I$=MID$(I$,2)+LEFT$(I$,1)
     NEXT JJ
     GOSUB 100
     IF II$<>LEFT$(ID$,LEN(II$)) THEN LOCATE 1,1:PRINT II$ " WAS NOT FOUND";:I$=II$:GOTO 2105
     LOCATE 1,1:PRINT SPC(78);
     GOSUB 2240
     GOTO 2100

2240 LOCATE LL,2
     EDITSPEC$= "  \   \ \             \ \          \  \\ ##ø##.## ###ø##.##  \     \   ###"
     PRINT USING EDITSPEC$;IDENT$(WAYPT);TYPE$(WAYPT);CTY$(WAYPT);ST$(WAYPT);LADEG(WAYPT);LAMIN(WAYPT);LODEG(WAYPT);LOMIN(WAYPT);FREQ$(WAYPT);MV(WAYPT)
     AIRSP(WAYPT) = KTS
     IF MV(WAYPT) = -128 THEN MV(WAYPT)=0
     WAYPT=WAYPT+1
     IF ALL < WAYPT THEN ALL=WAYPT
     REM IF WAYPT=2 THEN I=0:II=1:GOSUB 8000:PRINT:PRINT TC(I),DS(I):WAYPT=0
     LL=LL+1
     RETURN

2700 REM clear prompt "?" character
     LOCATE LL,1:PRINT " ";
     RETURN

2800 REM SAVE ROUTE
     IF ALL = 0 THEN INPUT"Nothing to save, press ENTER to continue";I:RETURN
     GOSUB 7900:IF F$="" THEN RETURN
     OPEN "O",#1,F$
     WRITE #1,ALL
     FOR WAYPT=0 TO ALL-1
        WRITE#1,IDENT$(WAYPT),PL$(WAYPT),AIRSP(WAYPT),WS(WAYPT),WD(WAYPT)
     NEXT WAYPT
     CLOSE #1
     RETURN

2900 REM RECALL ROUTE
     GOSUB 7900:IF F$="" THEN RETURN
     OPEN "I",#1,F$
     INPUT #1,ALL:WAYPT=ALL-1
     FOR WAYPT=0 TO ALL-1
        INPUT#1,IDENT$(WAYPT),PL$(WAYPT),AIRSP(WAYPT),WS(WAYPT),WD(WAYPT)
        I$=IDENT$(WAYPT)
        GOSUB 100
     NEXT WAYPT
     CLOSE #1
     RETURN

3000 REM cursor input handler
3010 I$=""
3020 LOCATE LL,1:PRINT "? ";I$;SPC(5-LEN(I$));:LOCATE LL,LEN(I$)+3,1
3025 II$=INKEY$:IF II$="" THEN 3025
     IF ASC(II$) = 0 THEN 3100
     IF II$ > "Z" THEN II$=CHR$(ASC(II$)-32)
     IF ASC(II$)=13 OR ASC(II$)=27 THEN 2700
     IF ASC(II$) <> 8 THEN 3060
     IF LEN(I$) <= 1 THEN 3010 ELSE I$=LEFT$(I$,LEN(I$)-1)
     GOTO 3080
3060 IF II$<"0" OR II$>"Z" OR (II$>"9" AND II$<"A") THEN 3025
     I$=I$+II$
3080 GOTO 3020

3100 REM extended key codes
     II=ASC(MID$(II$,2,1))
     IF II<> 72 THEN 3160

     REM CURSOR UP
     GOSUB 2700
     IF WAYPT < ALL THEN GOSUB 100:GOSUB 2240:LL=LL-1:WAYPT=WAYPT-1
     LL=LL-1:WAYPT=WAYPT-1:I$=IDENT$(WAYPT)
     GOTO 3020

3160 IF II<> 80 THEN 3200

     REM CURSOR DOWN
     GOSUB 2700
     IF WAYPT=ALL THEN 3020
     GOSUB 100:GOSUB 2240
     I$=IDENT$(WAYPT):GOTO 3020

3200 IF II<>83 THEN 3300

     REM delete
     IF WAYPT=ALL AND I$="" THEN 3020
     IF WAYPT=ALL THEN 3010
     ALL=ALL-1:OLD=WAYPT:FOR WAYPT = OLD TO ALL-1
     I$=IDENT$(WAYPT+1):GOSUB 100
     AIRSP(WAYPT)=AIRSP(WAYPT+1):WS(WAYPT)=WS(WAYPT+1):WD(WAYPT)=WD(WAYPT+1)
     LOCATE ,2
     PRINT USING EDITSPEC$;IDENT$(WAYPT);TYPE$(WAYPT);CTY$(WAYPT);ST$(WAYPT);LADEG(WAYPT);LAMIN(WAYPT);LODEG(WAYPT);LOMIN(WAYPT);FREQ$(WAYPT);MV(WAYPT)
     NEXT:IDENT$(WAYPT)="":WAYPT=OLD:LOCATE ,2:PRINT SPC(79);
     I$=IDENT$(WAYPT):GOTO 3020

3300 IF II<>82 THEN 3400

     REM insert
     IF WAYPT=ALL AND I$="" THEN 3020
     IF WAYPT=ALL THEN 3010
     OLD=WAYPT:FOR WAYPT = ALL+1 TO OLD+1 STEP -1
     IDENT$(WAYPT)=IDENT$(WAYPT-1)
     AIRSP(WAYPT)=AIRSP(WAYPT-1):WS(WAYPT)=WS(WAYPT-1):WD(WAYPT)=WD(WAYPT-1):NEXT
     LOCATE ,2:PRINT SPC(79):ALL=ALL+1:FOR WAYPT = OLD+1 TO ALL-1
     LOCATE ,2:I$=IDENT$(WAYPT):GOSUB 100
     PRINT USING EDITSPEC$;IDENT$(WAYPT);TYPE$(WAYPT);CTY$(WAYPT);ST$(WAYPT);LADEG(WAYPT);LAMIN(WAYPT);LODEG(WAYPT);LOMIN(WAYPT);FREQ$(WAYPT);MV(WAYPT)
     NEXT:WAYPT=OLD:IDENT$(WAYPT)="":
     I$=IDENT$(WAYPT):GOTO 3020

3400 GOTO 3020

4000 REM Invert a route
     IF ALL = 0 THEN INPUT"Nothing to invert, press ENTER to continue";I:RETURN
     FOR I=0 TO INT((ALL-1)/2):J=ALL-1-I
     SWAP PL$(I),PL$(J)
     SWAP IDENT$(I),IDENT$(J)
     SWAP FREQ$(I),FREQ$(J)
     SWAP MV(I),MV(J)
     SWAP ELEV(I),ELEV(J)
     SWAP LADEG(I),LADEG(J)
     SWAP LAMIN(I),LAMIN(J)
     SWAP LODEG(I),LODEG(J)
     SWAP LOMIN(I),LOMIN(J)
     SWAP AIRSP(I),AIRSP(J)
     SWAP WS(I),WS(J)
     SWAP WD(I),WD(J)
     SWAP TYPE$(I),TYPE$(J)
     SWAP CTY$(I),CTY$(J)
     SWAP ST$(I),ST$(J)
     NEXT I
     RETURN

7900 REM GET NAME OF NAVIGATION FILE
     ON ERROR GOTO 7990
     LOCATE LL+2,1:PRINT"Directory listing of":FILES"*"+EXT$
7915 ON ERROR GOTO 0
7920 LOCATE LL,1:PRINT"Enter a navigation route file name ("EXT$" is default)";
     INPUT F$
     IF LEN(F$)=0 THEN RETURN
     III=0:FOR II=1 TO LEN(F$):IF MID$(F$,II,1) = "." THEN IF III>0 THEN 7920 ELSE III=II
     NEXT II
     IF III > 9 OR (III=0 AND II > 9) THEN 7920
     IF III > 0 AND LEN(F$)-III > 3 THEN 7920
     IF III = 0 THEN F$=F$+EXT$
     RETURN
7990 RESUME 7915

8000 REM NAGIVATION CALCULATIONS from Waypoint I to II
     CLAI=LADEG(I)+(LAMIN(I)/60)
     CLOI=LODEG(I)+(LOMIN(I)/60)
     CLAIP1=LADEG(II)+(LAMIN(II)/60)
     CLOIP1=LODEG(II)+(LOMIN(II)/60)
     IF CLOI > 0 AND CLOIP1 < 0 THEN CLOIP1 = CLOIP1 + 360
     IF CLOI < 0 AND CLOIP1 > 0 THEN CLOIP1 = CLOIP1 - 360
     C1 = PI * (CLOIP1-CLOI): REM delta longitude
     IF CLOI = CLOIP1 AND CLAIP1 > CLAI THEN CS = 0 :GOTO 8100
     IF CLOI = CLOIP1 AND CLAIP1 > CLAI THEN CS = 180 :GOTO 8100
     C2 = LOG (TAN ((45 + (CLAIP1/2)) /DG ))
     C3 = LOG (TAN ((45 + (CLAI/2)) /DG ))
     IF C2=C3 THEN C=ATN(C1/.0000001) ELSE C = ATN(C1/(180 * (C3-C2)))
     CS = C * DG
8100 REM
     IF CLAI = CLAIP1 THEN DS=60*(CLOIP1-CLOI)*COS(CLAIP1/DG) ELSE DS=60*(CLAIP1-CLAI)/COS(C)
8120 IF CS < 0 THEN CS = CS+180:DS = -DS:GOTO 8120
8130 IF CS > 360 THEN CS = CS-180:DS = -DS:GOTO 8130
8140 IF DS < 0 AND CS > 180 THEN DS = -DS:CS = CS-180:GOTO 8140
8150 IF DS < 0 AND CS < 180 THEN DS = -DS:CS = CS+180:GOTO 8150
     IF CLOIP1>CLOI AND CS<180 THEN CS=CS+180 :GOTO 8170
     IF CLOIP1<CLOI AND CS>180 THEN CS=CS-180
8170 IF CS < 0 THEN CS = CS+360
     IF CS > 360 THEN CS = CS-360
     TC(II) = CS: DS(II) = DS
     RETURN

8200 REM compute wind correction factor
     IF AIRSP(I) <= 0 THEN GS=0:DRIFT=0:GOTO 8270
     OMEGA=WD(I)-TC(I):IF OMEGA >= 360 THEN OMEGA = OMEGA-360
     IF OMEGA < 0 THEN OMEGA = OMEGA+360
     OMEGA = OMEGA/DG
     CB=-2*WS(I)*COS(OMEGA):CC=WS(I)^2-AIRSP(I)^2
     CQ=SQR(CB^2-4*CC)
     GS1 = (-CB+CQ)/2:GS2=(-CB-CQ)/2
     IF ABS(GS2)>ABS(GS1) THEN GSMX=ABS(GS2) ELSE GSMX=ABS(GS1)
     IF ABS(GS2)<=ABS(GS1) THEN GSMI=ABS(GS2) ELSE GSMI=ABS(GS1)
     IF COS(OMEGA) < 0 THEN GS=GSMX ELSE GS=GSMI
     SINPHI=(WS(I)/GS)*SIN(OMEGA)
     DRIFT=ATN(WS(I)*SIN(OMEGA)/((AIRSP(I)^2+GS^2-WS(I)^2)/(2*GS)))*DG
     IF I=1 THEN CLIMBMIN = (CALT-ELEV(0))/FPM
     TM=DS(I)/GS*60:IF TTM > CLIMBMIN THEN 8258 ELSE TM=TM/CLIMBSP
     IF TTM+TM < CLIMBMIN THEN GS=GS*CLIMBSP:GOTO 8258
     TM=(DS(I)-(GS*CLIMBSP/60*(CLIMBMIN-TTM)))/GS*60+(CLIMBMIN-TTM)
8258 TTM=TTM+TM:TDS=TDS+DS(I)
     HR=INT(TM/60):MIN=INT(TM-HR*60)
     THR=INT(TTM/60):TMIN=INT(TTM-THR*60)
8270 TH=TC(I)+DRIFT:IF TH < 0 THEN TH=TH+360 ELSE IF TH>360 THEN TH=TH-360
     MH=TC(I)+DRIFT+(MV(I)+MV(I-1))/2:IF MH<0 THEN MH=MH+360 ELSE IF MH>360 THEN MH=MH-360
     RETURN

9000 REM Output the Flight plan
     PSTART=0

     REM Assume output is to the Computer screen
     OUTDEV%=0
     SUPER$=""
     LF$=""
     LL=25
     ON ERROR GOTO 9025
     IF LEFT$(OUTPUT$,3) <> "LPT" THEN OPEN "O",#1,OUTPUT$ ELSE OPEN "R",#1,OUTPUT$
     IF OUTPUT$="SCRN:" THEN CLS:GOTO 9030
     IF RIGHT$(OUTPUT$,1)=":" THEN
        WIDTH #1,255
        LF$=CHR$(13)+CHR$(10)
        LL=50
        OUTDEV%=1
        SUPER$=PRSUPER$
        PRINT#1,PRINIT$;
     ELSE LL=10000
     ENDIF

     GOTO 9030
9025 PRINT"Error trying to write to ";OUTPUT$
     OUTPUT$=""
     ON ERROR GOTO 9028
     CLOSE #1
9028 RESUME 9040

9030 REM Print Using specifications for 1st line
     IF SUPER$="" THEN
          PRTSPEC00$ = "\                      \  ###   ###" : REM Departure
          PRTSPEC10$ = "\                      \  ###   ###      ###         ###      #####.#   #####.#"
     ELSE
          PRTSPEC00$ = "\         \               ###   ###" : REM Departure
          PRTSPEC10$ = "\         \               ###   ###      ###         ###      #####.#   #####.#"
     ENDIF

     REM Print Using specification for 2nd line ...
     PRTSPEC01$ = "\   \ \    \   ##,###     ### @ ###" : REM Departure
     PRTSPEC11$ = "\   \ \    \   ##,###     ### @ ###      ###         ###       \    \    \    \"

9040 ON ERROR GOTO 0
     RETURN

9050 PSTART=PSTART+4
     FUZZY$=STRING$(79,"=")+LF$
     PRINT#1,FUZZY$
     PRINT#1,"WAYPOINT      POSITION   TRUE AS  GS   TRUE COURSE  MAG COURSE  DISTANCE  TOTAL";:PRINT#1,LF$
     PRINT#1,"IDENT FREQ    ELEVATION  WINDS         TRUE HEAD.   MAG HEAD.   TIME      TOTAL";:PRINT#1,LF$
     PRINT#1,FUZZY$
9090 RETURN

9100 IF I > 0 THEN MC=TC(I)+(MV(I)+MV(I-1))/2:IF MC<0 THEN MC=MC+360 ELSE IF MC > 360 THEN MC=MC-360
     LA$=RIGHT$(STR$(LADEG(I)),3)+DEGSYM$(OUTDEV%)
     LA$=LA$+RIGHT$(STR$(INT(LAMIN(I)+100)),2)+"."+RIGHT$(STR$(LAMIN(I)*100),2)
     LO$=RIGHT$(STR$(LODEG(I)),3)+DEGSYM$(0)
     LO$=LO$+RIGHT$(STR$(INT(LOMIN(I)+100)),2)+"."+RIGHT$(STR$(LOMIN(I)*100),2)

     PSTART=PSTART+4
     IF PSTART > LL THEN
        IF OUTPUT$<>"SCRN:" THEN
           PRINT#1,CHR$(12);
        ELSE
           PRINT #1,SPC(70);"More...";:INPUT PSTART
        ENDIF
        PSTART=4
     ENDIF

     IF SUPER$ <> "" THEN
        REM IF RIGHT$(OUTPUT$,1)=":" THEN WIDTH #1,255
        PRINT#1,SPC(13);SUPER$;LA$;CHR$(13);SPC(13);SUB$;LO$;CHR$(13);NORMAL$;
        REM IF RIGHT$(OUTPUT$,1)=":" THEN WIDTH #1,132
     ENDIF

     IF I=0 THEN
        PRINT#1,USING PRTSPEC00$;PL$(I);AIRSP(I),GS;
     ELSE
        PRINT#1,USING PRTSPEC10$;PL$(I);AIRSP(I),GS;TC(I),MC;DS(I);TDS;
        ENDIF
     PRINT#1,LF$

     ETE$=RIGHT$("      "+STR$(HR)+":"+RIGHT$(STR$(MIN+100),2),6)
     TTER$=RIGHT$("      "+STR$(THR)+":"+RIGHT$(STR$(TMIN+100),2),6)

     IF I=0 THEN
        PRINT#1,USING PRTSPEC01$;IDENT$(I);FREQ$(I);ELEV(I);WD(I);WS(I);
     ELSE
        PRINT#1,USING PRTSPEC11$;IDENT$(I);FREQ$(I);ELEV(I);WD(I);WS(I);TH;MH;ETE$;TTER$;
     ENDIF
     PRINT#1,LF$

     FUZZY$=STRING$(79,"-")+LF$
     PRINT#1,FUZZY$
     RETURN

10000 REM Display flight plan
10005 OUTPUT$="SCRN:":TTM=0:TDS=0
      INPUT"Output (ENTER for Screen, P for Printer, or FileName)";I$
      IF I$ <> "" THEN OUTPUT$ = I$
      IF I$="P" OR I$="p" THEN OUTPUT$ = DFPRTR$

      REM Relookup the departure to get all the details of the airport
      I$=IDENT$(0):GOSUB 10300

      GOSUB 9000
      IF OUTPUT$ = "" THEN PRINT"Output aborted.":GOTO 10050

      IF RNWY$ <> "" THEN GOSUB 10100
      GOSUB 9050

      FOR I=0 TO ALL-1
        II=I+1:GOSUB 8000:REM                   COMPUTE NAVIGATION INFORMATION
        IF I>0 THEN GOSUB 8200:REM              COMPUTE WIND CORRECTIONS
        IF I=0 THEN GS=CLIMBSP*AIRSP(I)
        GOSUB 9100:REM                          PRINT THE INFO
      NEXT I

      REM Compute an average ground speed that could be used for fuel swags
      AVGGS = INT((60 * TDS)/TTM)

      REM Relookup the destination to get all the details of the airport
      I$=IDENT$(ALL-1):GOSUB 10300

      I=ALL
      PL$(I)="App & Land"
      MV(I)=MV(I-1)
      WD(I)=WD(I-1):IF WD(I) = 0 THEN WD(I) = RUNWYHD
      WS(I)=WS(I-1)

      REM Pick a best runway based on the Wind direction
      MC=RUNWYHD

      IF RNWY$ <> "" THEN

         I$=RNWY$
         JJ = 360
         WHILE INSTR(1,I$,"(") > 0
            II = INSTR(1,I$,"(")
            III = INSTR(1,I$,")")
            II$ = MID$(I$,II+1,III-II-1)

REM PRINT "A runway: ";II$;" ",
            REM See what the cross wind direction is for this runway
            II = INSTR(1,II$,"/")
            J = VAL(LEFT$(II$,II-1))*10 : II$ = MID$(II$,II+1)
            GOSUB 10060
REM PRINT " ",

            J = VAL(II$)*10
            GOSUB 10060
REM PRINT

            REM Continue with all the runways
            I$ = MID$(I$,III+1)
         WEND
REM  PRINT RNWY$
REM  PRINT "Picked";MC;" For wind";WD(I)
REM  INPUT"Continue";ABC$
      ENDIF

      TC(I)=MC-MV(I)
      LADEG(I)=LADEG(I-1):LODEG(I)=LODEG(I-1)
      LAMIN(I)=LAMIN(I-1):LOMIN(I)=LOMIN(I-1)
      ELEV(I)=ELEV(I-1):FREQ$(I)=FREQ$(I-1)
      AIRSP(I)=2*AIRSP(I-1)/3:DS(I)=AIRSP(I)*5/60:GOSUB 8200
      IDENT$(ALL)=""
      GOSUB 9100

      IF RNWY$ <> "" THEN GOSUB 10100

      IF OUTPUT$<>"SCRN:" THEN
         PRINT#1,CHR$(12);
      ENDIF

10050 CLOSE #1
      INPUT"Press ENTER to continue";I
      RETURN

10060 REM Normalize crosswind angle for a runway
      II = ABS(J - WD(I))
      IF II >= 180 THEN IF J > WD(I) THEN II = ABS(WD(I) - J + 360) ELSE II = ABS(WD(I) - J - 360)
REM PRINT "Wind/angle";WD(I);"/";II;
      IF II < JJ THEN JJ = II:MC = J
      RETURN

10100 REM Print airport details
      III=12
      II$="":III$=APT$+" "+CITY$+" "+STATE$:GOSUB 10200
      IF APPROACH$<>"" THEN II$="Approach:":III$=APPROACH$:GOSUB 10200
      IF ATIS$<>"" THEN II$="ATIS:":III$=ATIS$:GOSUB 10200
      IF TWR$<>"" THEN II$="Tower/CTAF:":III$=TWR$:GOSUB 10200
      IF GROUND$<>"" THEN II$="Ground:":III$=GROUND$:GOSUB 10200
      IF TFREQS$<>"" THEN
         II=INSTR(1,TFREQS$+" "," "):III$=MID$(TFREQS$,II+1)
         IF III$<> "" THEN II$="Other:":GOSUB 10200
      ENDIF
      IF RNWY$<>"" THEN II$="Runways:":III$=RNWY$:GOSUB 10200
      IF FUELINFO$<>"" THEN II$="FBO:":III$=FUELINFO$:GOSUB 10200
      RETURN

10200 REM Print a string II$ as the title for section III$
      REM Print a string (at tab III) broken on multiple lines at a ")" or " "
      I = 80 - III
      IF PSTART + LEN(III$)/I + 1 >= LL THEN
         IF OUTPUT$<>"SCRN:" THEN
            PRINT#1,CHR$(12);
         ELSE
            PRINT #1,SPC(70);"More...";:INPUT PSTART
         ENDIF
         PSTART=0
      ENDIF

      PRINT#1,II$;
      WHILE LEN(III$) > 0
         IF LEN(III$) >= I THEN
            II = 1:IJ=1
10210       IJ = INSTR(II,III$,")"):IF IJ < I AND IJ > 0 THEN II=IJ+1:GOTO 10210

            IF II = 1 THEN
10220          IJ = INSTR(II,III$," "):IF IJ < I AND IJ > 0 THEN II=IJ+1:GOTO 10220
            ENDIF
            PRINT#1,TAB(III);LEFT$(III$,II-1);
            III$=MID$(III$,II)
            WHILE LEFT$(III$,1) = " "
               III$=MID$(III$,2)
            WEND
         ELSE
            PRINT#1,TAB(III);III$;
            III$=""
         ENDIF
         PRINT#1,LF$
         PSTART=PSTART+1
      WEND
      RETURN

10300 REM Re-lookup airport information for Departure or Destination
      GOSUB 100
      IF FILE%=3 AND II$=ID$ THEN GOSUB FMTAPT ELSE RNWY$=""
      RETURN

13000 REM INPUT of Winds Aloft
      PRINT:PRINT SPC(30);"Specific Trip":PRINT
      I=0:PRINT"Enter wind direction and speed for each leg"
      PRINT"   Press ENTER all by itself to use information from previous leg"
      PRINT"   Press = to use same information for remaining legs"
      PRINT"   (Pressing = on the first entry resets all legs to no winds.)"
      PRINT
13050 PRINT IDENT$(I);TAB(8);PL$(I);"  ";:LINE INPUT I$
      IF LEN(I$) <> 4 THEN 13200

      REM Data entered as 4 digit number, 1st two wind direction, last speed
      REM  i.e. 2010  for 200 Degrees at 10 Kts
      WD(I)=VAL(LEFT$(I$,2))*10
      WS(I)=VAL(RIGHT$(I$,2))
      GOTO 13500

13200 IF I$<> "" THEN 13300

      REM Just ENTER all by itself, use previous leg's data
      IF I=0 THEN WD(I)=0:WS(I)=0:GOTO 13500
      WD(I)=WD(I-1):WS(I)=WS(I-1)
      GOTO 13500

13300 IF I$<> "=" THEN 13400

      REM Use same information for all remaining legs
      IF I=0 THEN WD(I)=0:WS(I)=0:I=I+1
      FOR JJ = I TO ALL
      WD(JJ)=WD(JJ-1):WS(JJ)=WS(JJ-1)
      NEXT JJ
      I=ALL
      GOTO 13500

      REM Direction and Speed entered separately (Comma or Blank delimited)
13400 JJ=0
13410 JJ=JJ+1:IF JJ<LEN(I$) AND MID$(I$,JJ,1) <> "," AND MID$(I$,JJ,1) <> " " THEN 13410
      WD(I)=VAL(LEFT$(I$,JJ-1))
      WS(I)=VAL(MID$(I$,JJ+1))

13500 I = I+1:IF I< ALL THEN 13050
      RETURN

14900 REM Invoke the "Set Defaults" program
      REM We will never "return" here, but will re-start this
      REM   program's execution from the beginning...
      CLOSE
      RUN "ALSE6BCF"

20160 REM Prompt for identifier
      PRINT
20165 INPUT"Identifier";I$
      GOSUB UPPERCASE:
      RETURN

20245 Rem Somewhat Common function.  DISPLAY ONLY an Airport
      GOSUB FMTAPT
      CHANGE$="FALSE":DTLCHANGE$="FALSE"
      CLS:PRINT SPC(30);"Basic E6B Calculations"
      PRINT TAB(28);"Airport Information Record":PRINT
      PRINT" id           ";ID$;TAB(35);" Revision Date ";CYCLE$
      PRINT" Tower / CTAF ";
      IF RIGHT$(TWR$,1)="0" THEN TWR$=LEFT$(TWR$,LEN(TWR$)-1)+" "
      IF RIGHT$(TWR$,2)="0 " THEN TWR$=LEFT$(TWR$,LEN(TWR$)-2)+"  "
      PRINT TWR$
      PRINT" city         ";CTY$
      PRINT" state        ";ST$
      PRINT" latitude     ";LAT$
      PRINT" longitude   ";LONG$
      PRINT " elev         ";
      PRINT USING "##,###";VAL(ELE$)
      PRINT " mag var        ";VAR$
      PRINT " Name         ";NAM$
      PRINT " Fuel         ";FBO$
      PRINT
      PRINT " FBO Details  ";FBONAME$
      PRINT " Runway(s)    ";RNWY$
      PRINT " ATIS/AWOS    ";ATIS$
      PRINT " Ground       ";GROUND$
      PRINT " Approach     ";APPROACH$
      PRINT " Frequencies+ ";TFREQS$
      PRINT:INPUT"Press 'ENTER' when done";I
      RETURN

20450 Rem Somewhat Common function.  DISPLAY ONLY a Navaid
      GOSUB FMTNAV
      CHANGE$="FALSE"
      CLS:PRINT SPC(30);"Basic E6B Calculations"
      PRINT TAB(28);"Navaid Information Record":PRINT
      PRINT" id              ";ID$
      PRINT" frequency       ";
      IF RIGHT$(TWR$,1)="0" THEN TWR$=LEFT$(TWR$,LEN(TWR$)-1)+" "
      IF RIGHT$(TWR$,2)="0 " THEN TWR$=LEFT$(TWR$,LEN(TWR$)-2)+"  "
      PRINT TWR$
      PRINT" city            ";CTY$
      PRINT" state           ";ST$
      PRINT" latitude        ";LAT$
      PRINT" longitude      ";LONG$
      PRINT " elev            ";
      PRINT USING "##,###";VAL(ELE$)
      PRINT " mag var           ";VAR$
      PRINT " Name            ";NAM$
      PRINT " Revision Date   ";CYCLE$
      PRINT:INPUT"Press 'ENTER' when done";I
      RETURN

20650 Rem Somewhat Common function.  DISPLAY ONLY a Navigational Fix
      GOSUB FMTFIX
      CHANGE$="FALSE"
      CLS:PRINT SPC(30);"Basic E6B Calculations"
      PRINT TAB(28);"Navigation Fix Information Record":PRINT
      PRINT" id               ";ID$
      PRINT" type             ";NAM$
      PRINT" state            ";ST$
      PRINT" latitude         ";LAT$
      PRINT" longitude       ";LONG$
      PRINT" mag var         ";VAR$
      PRINT" reference        ";REF1$
      PRINT"   radial         ";RADIAL
      PRINT"   distance       ";DIST
      PRINT" Revision Date   ";CYCLE$
      PRINT
      PRINT:INPUT"Press 'ENTER' when done";I
      RETURN

20800 REM Work with 1st record that matches the id
      RESORT=0
      PRINT"Work with 1st record that matches id"
      GOSUB 20160
      IF II$="" THEN RETURN
      WAYPT=WAYPT+1:I$=II$:GOSUB 100:WAYPT=WAYPT-1:RECNUM=III
      IF II$<>ID$ THEN INPUT "Not found. Press ENTER to continue";I$:GOTO 20800
      ON FILE%-1 GOSUB 20450,20245,20650
      RETURN

UPPERCASE: REM Convert I$ to upper case returning II$
      II$="":FOR I=1 TO LEN(I$)
      IF MID$(I$,I,1) >= "a" AND MID$(I$,I,1) <= "z" THEN II$=II$+CHR$(ASC(MID$(I$,I,1))-32) ELSE II$=II$+MID$(I$,I,1)
      NEXT I
      RETURN

22000 REM
22005 CLS:PRINT SPC(30);"Basic E6B Calculations"
      PRINT TAB(35);"Query Database":PRINT
      PRINT" 1 - Specify databases to search (";
        I = RFILES%
        WHILE I > 0
           II = I - 10 * INT(I/10)
           PRINT " ";FN$(II);
           I = INT(I/10)
        WEND
        PRINT " )"
      PRINT
      PRINT" 2 - Vicininty of a point"
      PRINT" 3 - Local fuel prices around a point"
      PRINT
      PRINT" 4 - Points along a route"
      PRINT" 5 - Fuel cost SWAG along route"
      PRINT
      PRINT" 6 - Fuzzy search"
      PRINT" 7 - Lookup by ID"
      PRINT
      INPUT"selection";I
      IF I = 0 THEN RETURN ELSE IF I > 7 THEN 22005

      REM Set some common query defaults to save code space
      RESORT=0
      QDEPART = 0  :REM
      QSTATE=0     :REM Don't want state displayed (unless FUZZY search)
      QCITY=0      :REM Don't usually want to display city
      QF=0         :REM FILE% of data base matched
      QLEG=0       :REM Distance "TO" the point
      QTDS=0       :REM Total distance
      QDELTA=0     :REM Delta distance
      QFUEL=0      :REM Fuel Price
      QCOST=0      :REM Extended cost
      QROUTE=0     :REM We aren't doing routing calculations
      SF=1
      SF$(1)="Distance":SF%(1)=2

      ON I GOSUB 22900,22100,22200,22300,22400,22500,20800

      GOTO 22005

22050 REM Prompt for 1st point
      GOSUB 20165
      IF II$="" THEN RETURN
      WAYPT=WAYPT+1:QDEPART=WAYPT:I$=II$:GOSUB 100:WAYPT=WAYPT-1
      IF II$<>LEFT$(ID$,LEN(II$)) THEN PRINT III$;" Not found":GOTO 22050
      PRINT ID$;" - ";APT$,STATE$

      QRECT!(0,0) = LADEG(QDEPART)+(LAMIN(WAYPT+1)/60) - 2
      QRECT!(0,1) = LODEG(QDEPART)+(LOMIN(WAYPT+1)/60) - 2

      QRECT!(1,0) = QRECT!(0,0) + 4
      QRECT!(1,1) = QRECT!(0,1) + 4

      QDESTIN = QDEPART+1  :REM Interested in distances around point
      RETURN

22060 REM Prompt for Destination
      JJ = 3
      QRECT!(1,0) = QRECT!(0,0)
      QRECT!(1,1) = QRECT!(0,1)
22065 PRINT
      PRINT "Hints:  Enter '+id' for a soft 'approximate' waypoint"
      PRINT "              '=id' for a hard waypoint you must fly to"
      PRINT "              'id' for the final destination."
      PRINT "          or press ENTER by itself to respecify previous entry."
      PRINT
      PRINT "Destination ";
      GOSUB 20165
      IF II$="" THEN RETURN

      REM Assume a "Hard" destination endpoint that must be flown
      QRTEWPT%(WAYPT+JJ) = QENDPT%

      REM Is this just an intermediate point?
      IF LEFT$(II$,1) = "=" THEN QRTEWPT%(WAYPT+JJ) = QHRDPT%:II$=MID$(II$,2)
      IF LEFT$(II$,1) = "+" THEN QRTEWPT%(WAYPT+JJ) = QSFTPT%:II$=MID$(II$,2)

      WAYPT=WAYPT+JJ:I$=II$:GOSUB 100:QDESTIN=WAYPT:WAYPT=WAYPT-JJ
      IF II$<>LEFT$(ID$,LEN(II$)) THEN PRINT III$;" Not found":GOTO 22065
      PRINT ID$;" - ";APT$,STATE$

      CLAI = LADEG(QDESTIN)+(LAMIN(QDESTIN)/60)
      CLOI = LODEG(QDESTIN)+(LOMIN(QDESTIN)/60)

      IF CLAI < QRECT!(0,0) THEN QRECT!(0,0) = CLAI
      IF CLAI > QRECT!(1,0) THEN QRECT!(1,0) = CLAI
      IF CLOI < QRECT!(0,1) THEN QRECT!(0,1) = CLOI
      IF CLOI > QRECT!(1,1) THEN QRECT!(1,1) = CLOI

      IF QRTEWPT%(WAYPT+JJ) <> QENDPT% THEN JJ=JJ+1:GOTO 22065

      PRINT:INPUT "Continue";I$
      IF LEFT$(I$,1) = "N" OR LEFT$(I$,1) = "n" THEN 22065

      REM Allow for some deviation
      QRECT!(0,0) = QRECT!(0,0) - .5
      QRECT!(0,1) = QRECT!(0,1) - .5
      QRECT!(1,0) = QRECT!(1,0) + .5
      QRECT!(1,1) = QRECT!(1,1) + .5

      RETURN

22090 REM Perform the search and display the results

      WAYPT=WAYPT+2
      GOSUB 22700

      REM Display the results
      GOSUB 22800

      WAYPT=WAYPT-2

      RETURN

22100 REM Query points around another point
      GOSUB 22050
      IF II$="" THEN RETURN

      INPUT "Continue";I$
      IF LEFT$(I$,1) = "N" OR LEFT$(I$,1) = "n" THEN 22100

      REM Define the things to query
      QF=1      :REM FILE% of data base matched
      QDELTA=2  :REM Delta distance

      REM Search and Display
      GOSUB 22090
      RETURN

22200 REM Query fuel availability around a point
      GOSUB 22050
      IF II$="" THEN RETURN

      INPUT "Continue";I$
      IF LEFT$(I$,1) = "N" OR LEFT$(I$,1) = "n" THEN 22200

      REM Define the things to query
      RFILES% = 3  :REM It only makes sense to query Airports
      QDELTA=2     :REM Delta distance
      QFUEL=3      :REM Fuel Price
      SF=2
      SF$(2)="Fuel Price":SF%(2)=3
      QCITY = 10  :REM Display City

      REM Search and Display
      GOSUB 22090
      RETURN

22300 REM Query points along route
      PRINT:PRINT "Departure ";
      GOSUB 22050
      IF II$="" THEN RETURN

      QRECT!(0,0) = LADEG(QDEPART)+(LAMIN(QDEPART)/60)
      QRECT!(0,1) = LODEG(QDEPART)+(LOMIN(QDEPART)/60)

      REM Ask for the Destination
      GOSUB 22060
      IF II$="" THEN 22300

      REM Define the things to query
      QF=1      :REM FILE% of data base matched
      QLEG=2    :REM Distance "TO" the point
      QTDS=3    :REM Total distance
      QDELTA=4  :REM Delta distance
      SF=3:
      SF$(1)="Distance To":SF%(1)=2
      SF$(2)="Delta route distance":SF%(2)=4
      SF$(3)="Type of point":SF%(3)=1

      REM Search and Display
      GOSUB 22090

      RETURN

22400 REM Query fuel prices along route
      PRINT:PRINT "Departure ";
      GOSUB 22050
      IF II$="" THEN RETURN

      QRECT!(0,0) = LADEG(QDEPART)+(LAMIN(QDEPART)/60)
      QRECT!(0,1) = LODEG(QDEPART)+(LOMIN(QDEPART)/60)

      PRINT"Fuel on board prior to departure (";FUELLOAD;" gallons)";
      INPUT I
      IF I > 0 THEN FUELLOAD = I

      REM Ask for the Destination
      GOSUB 22060
      IF II$="" THEN 22400

      PRINT"Average ground speed (";AVGGS;" KTS)";
      INPUT I
      IF I > 0 THEN AVGGS = I

      PRINT"Fuel reserve desired (";FUELMIN;" gallons)";
      INPUT I
      IF I > 0 THEN FUELMIN = I

      REM Define the things to query
      RFILES% = 3
      QLEG=1       :REM Distance "TO" the point
      QTDS=2       :REM Total distance
      QDELTA=3     :REM Delta distance
      QFUEL=4      :REM Fuel Price
      QROUTE=5     :REM Save the possible routing used in the calculations
      QCOST=6      :REM Estimate the cost of the proposed routing
      SF=4:
      SF$(1)="Distance To":SF%(1)=QLEG
      SF$(2)="Delta route distance":SF%(2)=QDELTA
      SF$(3)="Fuel Price":SF%(3)=QFUEL
      SF$(4)="Estimated Trip variable expense":SF%(4)=QCOST

      PRINT "Searching along route ";IDENT$(QDEPART);
      FOR I = QDEPART+2 TO QDESTIN
         PRINT " - ";IDENT$(I);
      NEXT I
      PRINT

      REM Search and Display
      GOSUB 22090

      RETURN

22500 REM Fuzzy search
      PRINT:LINE INPUT "Enter search string? ";I$
      IF I$ = "" THEN RETURN

      GOSUB 22550
      FUZZY$ = II$

      REM Process each file
      CRESULTS = 0
      SF=0
      III = RFILES%
      WHILE III > 0
         GOSUB 22580
         FIELD #FILE%,KEYSIZE(FILE%) AS ID$, RECSIZE(FILE%)-KEYSIZE(FILE%) AS DATA$

         FOR RECNUM = 1 TO NREC(FILE%)
            IF RECNUM = 250 * INT(RECNUM/250) THEN PRINT ".";
            GET #FILE%,RECNUM

            I$ = DATA$
            GOSUB 22550

            IF INSTR(1,II$,FUZZY$) > 0 THEN GOSUB 22590

22530    NEXT RECNUM

      WEND
      FUZZY$ = ""

      REM Display the results
      QSTATE = 10:QCITY=10
      WAYPT=WAYPT+2
      GOSUB 22800
      WAYPT=WAYPT-2

      RETURN

22550 REM Given I$, Create FUZZY compare string II$
      GOSUB UPPERCASE
      III$ = II$ + " "
      II$ = ""
      FOR II = 1 TO LEN(III$)-1
          REM Eliminate duplicated letters, the common vowels, and spaces
          IF MID$(III$,II,1) = MID$(III$,II+1,1) THEN 22555
          FOR I = 1 TO LEN("AEIOU ,")
             IF MID$(III$,II,1) = MID$("AEIOU ,",I,1) THEN 22555
          NEXT I
          II$=II$+MID$(III$,II,1)
22555 NEXT II
      RETURN

22580 REM
      FILE% = III - 10 * INT(III/10)
      PRINT "Querying ";NREC(FILE%);FN$(FILE%);" ... please wait ..."
      III = INT(III/10)
      RETURN

22590 REM Save the ID into the Query Results
      CRESULTS = CRESULTS + 1
      IF CRESULTS < IMAXQUERY THEN 22595
         CRESULTS = CRESULTS - 1
         PRINT "Too many matches.  Returning first";IMAXQUERY -1;"matches"
         INPUT "Continue";YUP$
         RECNUM = NREC(FILE%)
         RETURN
22595 QRESULT#(CRESULTS,0) = FNPKEY#(ID$)
      IF QF > 0 THEN QRESULT#(CRESULTS,QF) = FILE%
      IF QLEG > 0 THEN QRESULT#(CRESULTS,QLEG) = DS(WAYPT)
      IF QTDS > 0 THEN QRESULT#(CRESULTS,QTDS) = DSROUTE
      IF QDELTA > 0 THEN QRESULT#(CRESULTS,QDELTA) = DSROUTE - QRESULT#(0,3)
      IF QFUEL > 0 THEN QRESULT#(CRESULTS,QFUEL)=FUELP(WAYPT)
      IF QCOST > 0 THEN QRESULT#(CRESULTS,QCOST) = TRIPCOST
      IF QROUTE > 0 THEN QRESULT#(CRESULTS,QROUTE) = VAL(PROUTE$)

      RETURN

22700 REM Scan the selected file(s) for records that meet the criteria
      REM Input:  RFILES% - Each digit represents a file to scan
      REM         QRECT!(0..1,0..1) Lat,Long of rectangular area

      QRESULT#(0,3) = 0
      REM Compute the minimum distance along the route & save it
      JJ = QDEPART
      FOR II = QDEPART+2 TO QDESTIN
         I = JJ
         GOSUB 8000
         QRESULT#(0,3) = QRESULT#(0,3) + DS(II)
         JJ = II
      NEXT II

22710 CRESULTS = 0

      REM Compute the maximum corridor width allowed
      QDSMAXALLOW = QRESULT#(0,3) * QINCREASE
      IF QDSMAXALLOW - QRESULT#(0,3) < QROUTEWIDTH THEN QDSMAXALLOW = QRESULT#(0,3) + QROUTEWIDTH

      REM Process each file
      III = RFILES%
      WHILE III > 0
         GOSUB 22580
         ON FILE%-1 GOSUB 1120,1130,1140

         FOR RECNUM = 1 TO NREC(FILE%)
            IF RECNUM = 750 * INT(RECNUM/750) THEN PRINT ".";
            GET #FILE%,RECNUM
            IF LEFT$(ID$,1) = "~" THEN 22730
            IDENT$(WAYPT)=ID$
            FUELP(WAYPT) = CVI(FUEL$)/1000

            REM Unpack the Latitude/Longitude
            GOSUB 500
            CLAI=LADEG(WAYPT)+(LAMIN(WAYPT)/60)
            CLOI=LODEG(WAYPT)+(LOMIN(WAYPT)/60)

            IF CLAI < QRECT!(0,0) THEN 22730
            IF CLAI > QRECT!(1,0) THEN 22730
            IF CLOI < QRECT!(0,1) THEN 22730
            IF CLOI > QRECT!(1,1) THEN 22730

            REM Determine a possible routing that includes this waypoint
            AROUTE$="12"
            FOR I = QDEPART+2 TO QDESTIN
               AROUTE$ = AROUTE$+MID$(STR$(I-QDEPART+1),2)
            NEXT I

            REM For each permutation of the route with this waypoint
            FOR J = 2 TO LEN(AROUTE$)

               REM Compute along this proposed routing
               PROUTE$=AROUTE$
               GOSUB 22750
               IF DSROUTE <= QDSMAXALLOW THEN
                  IF FILE% <> 3 OR CVI(FUEL$) >= QFUEL THEN GOSUB 22590
               END IF

               AROUTE$=LEFT$(AROUTE$,J-1)+MID$(AROUTE$,J+1,1)+MID$(AROUTE$,J,1)+MID$(AROUTE$,J+2)
               IF RIGHT$(AROUTE$,1) = "2" THEN J=LEN(AROUTE$)+1
            NEXT J

22730    NEXT RECNUM

      WEND

      RETURN

22750 REM Compute distance along a proposed route "PROUTE$"
      DSROUTE = 0
      DSWAYPT = 0
      II$="N" : REM Internal flag, Have we reached potential waypoint "N/Y"

      REM Compute distance along this potential routing
      FOR JJ = 1 TO LEN(PROUTE$)-1
         I = VAL(MID$(PROUTE$,JJ,1))+QDEPART-1
         II = VAL(MID$(PROUTE$,JJ+1,1))+QDEPART-1
         GOSUB 8000

         REM Special case:  Suggested waypoint is the departure
         IF PROUTE$<>"12" AND DS(II) = 0 AND I = QDEPART THEN DS(II)=9999999: Rem cause route to be ignored

         REM If proposed point is near an optional intermediate waypoint
         IF DS(II) <= QROUTEWIDTH AND QRTEWPT%(II) = QSFTPT% AND I = QDEPART+1 THEN
            REM We can substitute the waypoint
            PROUTE$ = LEFT$(PROUTE$,JJ)+MID$(PROUTE$,JJ+2)
            GOTO 22750
         ENDIF

         REM Special case:  Suggested waypoint is the next intermediate waypoint
         IF DS(II) = 0 AND I > II THEN DS(II)=9999999: Rem cause route to be ignored

         DSROUTE=DSROUTE + DS(II)

         REM Keep track of total distance to the suggested waypoint
         IF II$ = "N" THEN DSWAYPT = DSWAYPT + DS(II)
         IF II = QDEPART+1 THEN II$="Y"

      NEXT JJ

      REM Are we asking for a rough estimate of the cost of this flight
      IF QCOST > 0 THEN GOSUB 22775

      REM Force the distance to the suggested waypoint to be total to waypoint
      DS(WAYPT) = DSWAYPT

      RETURN

22775 REM Compute cost along a proposed route "PROUTE$"
      TRIPCOST = 0
      FUELLEFT = FUELLOAD
      FUELADD(QDEPART) = 0
      FUELARRIVE(QDEPART) = FUELLEFT

      REM Compute possible fuel stops between I and II
      I = VAL(MID$(PROUTE$,1,1))+QDEPART-1
      JJ = 1

      DO WHILE JJ < LEN(PROUTE$)

         REM Search for the next possible landing point
         DSLEG = 0
         DO WHILE JJ <= LEN(PROUTE$)
            JJ  = JJ + 1
            II = VAL(MID$(PROUTE$,JJ,1))+QDEPART-1
            DSLEG = DSLEG + DS(II)
            FUELADD(II) = 0
            FUELARRIVE(II) = -1
            IF QRTEWPT%(II) <> QSFTPT% THEN EXIT DO
         LOOP

         REM Compute approximate time (in minutes) to cover the distance
         TM = DSLEG / AVGGS

         REM Is it more expensive at departure than at the proposed waypoint
         IF FUELP(I) > FUELP(II) AND FUELP(II) > 0 THEN

            REM Only put on enough fuel to reach the waypoint
            FUELNEED = (TM * FUELBURN) + (FUELBURN * .15) + FUELMIN
            IF FUELNEED > FUELCAP THEN
               FUELADD(I) = FUELCAP - FUELLEFT
            ELSE
               IF FUELNEED > FUELLEFT THEN
                  FUELADD(I) = FUELNEED - FUELLEFT
               ENDIF
            ENDIF

         REM Else departure is cheaper, so top off here
         ELSE IF FUELP(I) > 0 THEN FUELADD(I) = FUELCAP - FUELLEFT

         ENDIF

         REM Accumulate the costs associated with departing this location
         TRIPCOST = TRIPCOST + (FUELADD(I) * FUELP(I))
         FUELLEFT = FUELLEFT + FUELADD(I)

         REM Allow some fuel to be used up for takeoff
         FUELUSEABLE(I) = FUELLEFT
         FUELLEFT = FUELLEFT - (FUELBURN * .15)

         REM Compute how much fuel we will use to get to the waypoint
         FUELLEFT = FUELLEFT - (TM * FUELBURN)
         FUELUSEABLE(II) = FUELLEFT

         REM Running out of fuel is real Expensive! (Note account for rounding)
         FUELARRIVE(II) = FUELLEFT
         IF FUELLEFT < (FUELMIN - .001) THEN TRIPCOST = TRIPCOST + 10000

         REM And it costs us some for hourly expenses
         TRIPCOST = TRIPCOST + (TM * HOURLY)

         REM If we are below minimum legal fuel, disregard this flight plan
         IF FUELLEFT < FUELBURN * .5 THEN DSROUTE = DSROUTE+100000:RETURN

         REM Set up for the next flight leg
         I = II
      LOOP

      REM Assume we top off at the final destination after arrival
      IF FUELP(QDESTIN) > 0 THEN
         FUELADD(QDESTIN) = FUELCAP - FUELLEFT

         TRIPCOST = TRIPCOST + (FUELADD(QDESTIN) * FUELP(QDESTIN))
         FUELUSEABLE(QDESTIN) = FUELLEFT + FUELADD(QDESTIN)

      ENDIF

      RETURN

22800 PSTART = 1
      OUTPUT$="SCRN:"
      FONE%=0           :REM Let the common code open the output device
      LF$=""
22805 CLS:PRINT "Query Results (";PSTART;"of";CRESULTS;")":PRINT

      JJ = 20
      GOSUB 22890
      LOCATE 24,1
      I$ = "Details of number"
      IF PSTART > 1 OR LSHOWN < CRESULTS THEN I$ = I$ + ", PgUp/PgDn"
      I$ = I$ + ", P to Print, F for File"
      IF SF>0 THEN I$ = I$ + ", S to Sort"
      LL = LEN(I$)+2
      LOCATE 24,1:PRINT I$;

22840 I$=""
22842 LOCATE 24,LL:PRINT "? ";I$;SPC(5-LEN(I$));:LOCATE 24,LL+LEN(I$)+2,1
22844 II$=INKEY$:IF II$="" THEN 22844
      REM Optimization:  Use Uppercased value of key as II
      II = ASC(II$):IF II > 90 THEN II = II - 32
      IF II = 0 THEN 22850
      IF II=70 or II=80 THEN 22880                  :REM "F" or "P"
      IF II=83 THEN CLOSE#1:GOSUB 23000:GOTO 22800  :REM "S"
      IF II = 27 THEN CLOSE#1:RETURN
      IF II = 13 THEN 22860
      IF II <> 8 THEN 22845
      IF LEN(I$) <= 1 THEN 22840 ELSE I$=LEFT$(I$,LEN(I$)-1)
      GOTO 22849
22845 IF II$<"0" OR II$>"9" THEN 22844
      IF LEN(I$) < LEN(STR$(CRESULTS)) THEN I$=I$+II$
22849 GOTO 22842

22850 REM extended key codes
      II=ASC(MID$(II$,2,1))

      IF II = 71 THEN PSTART = 1                                :REM HOME
      IF II = 80 THEN IF PSTART < CRESULTS THEN PSTART=PSTART+1 :REM Cursor Down
      IF II = 72 THEN IF PSTART > 1 THEN PSTART=PSTART-1        :REM Cursor Up
      IF II = 81 THEN IF LSHOWN < CRESULTS THEN PSTART=LSHOWN+1 :REM Pg Down
      IF II = 73 THEN PSTART = PSTART - JJ - 1                  :REM Pg Up
      IF II = 79 THEN PSTART = CRESULTS - JJ                    :REM END
      IF PSTART < 1 THEN PSTART=1
      GOTO 22805

22860 REM
      IJ = VAL(I$)
      IF IJ = 0 THEN CLOSE#1:RETURN
      IF IJ < PSTART OR IJ > PSTART + 20 THEN PSTART = IJ:GOTO 22805
      IF IJ > CRESULTS THEN 22805

      IF QF > 0 THEN FILE% = QRESULT#(IJ,QF)
      I$ = FNEKEY$(QRESULT#(IJ,0))
      GOSUB 400

      REM If we weren't not calculating a speculative route, just display details
      RECID$=ID$  :REM Used to signal if the Record ID Changed
      IF QROUTE = 0 THEN ON FILE%-1 GOSUB 20450,20245,20650:GOTO 22805

      REM We calculated a possible flight route, display those details
      PROUTE$  = MID$(STR$(QRESULT#(IJ,QROUTE)),2)
      GOSUB 22750

22870 PRINT:PRINT
      PRINT TAB(15);"Rough estimates of fuel (gallons)"
      FOR III = 1 TO LEN(PROUTE$)
          I = VAL(MID$(PROUTE$,III,1))+QDEPART-1
          IF I = QDEPART+1 THEN IDENT$(I) = FNEKEY$(QRESULT#(IJ,0))
          IF III < LEN(PROUTE$) OR (DSROUTE - DS(WAYPT)) > 0 THEN
             PRINT III;" - ";IDENT$(I);TAB(15);
             IF FUELARRIVE(I) > 0 THEN PRINT USING"Arrival: ###.#";FUELARRIVE(I);
             PRINT USING"Add: ###.#   Fuel on board: ###.#";TAB(33);FUELADD(I);FUELUSEABLE(I);
             PRINT
          ENDIF
      NEXT III
      PRINT:INPUT "Selection for waypoint details";III
      IF III = 0 THEN 22805
      I = VAL(MID$(PROUTE$,III,1))+QDEPART-1
      IF I = QDEPART+1 THEN I$ = FNEKEY$(QRESULT#(IJ,0)):GOSUB 400 ELSE I$=IDENT$(I):GOSUB 100
      ON FILE%-1 GOSUB 20450,20245,20650
      GOTO 22870

22880 REM Output to the printer/File
      OUTPUT$ = DFPRTR$
      IF II = 70 THEN
         PRINT:PRINT
         INPUT"Save as file name (ENTER defaults to printer)";I$
         IF I$ <> "" THEN OUTPUT$ = I$
      ELSE
         LF$=CHR$(13)+CHR$(10)
      ENDIF
      CLOSE #1
      IF LEFT$(OUTPUT$,3) <> "LPT" THEN OPEN "O",#1,OUTPUT$ ELSE OPEN "R",#1,OUTPUT$
      FONE%=1 :REM File #1 is open for output
      JJ = 10000
      IF QDEPART > 0 THEN
         PRINT #1,"Query of ";IDENT$(QDEPART);
         IF QDESTIN > QDEPART+1 THEN PRINT #1," to ";IDENT$(QDESTIN);
         PRINT #1,LF$
         PRINT #1,LF$
      ENDIF
      GOSUB 22890
      CLOSE #1
      FONE%=0
      GOTO 22800

22890 REM Print out the report
      IF FONE% = 0 THEN
         IF LEFT$(OUTPUT$,3) <> "LPT" THEN OPEN "O",#1,OUTPUT$ ELSE OPEN "R",#1,OUTPUT$
         FONE%=1
      ENDIF
      FOR ISTART = PSTART TO PSTART+JJ
         IF ISTART <= CRESULTS THEN
            LSHOWN = ISTART
            PRINT#1,RIGHT$("  "+STR$(ISTART),3);" - ";

            I$ = FNEKEY$(QRESULT#(ISTART,0))
            IF QF > 0 THEN FILE% = QRESULT#(ISTART,QF)
            GOSUB 400

            PRINT#1,USING "\   \ \                   \ ";ID$;APT$;

            IF QCITY > 0 THEN PRINT#1," ",CITY$;
            IF QSTATE > 0 THEN PRINT#1," "STATE$;
            IF QLEG > 0 THEN PRINT#1,USING "####";QRESULT#(ISTART,QLEG);
            IF QTDS > 0 THEN PRINT#1,USING "/####";QRESULT#(ISTART,QTDS);
            IF QDELTA > 0 THEN PRINT#1,USING " (+### nm)";QRESULT#(ISTART,QDELTA);

            IF QFUEL > 0 THEN PRINT#1,USING " ##.##";QRESULT#(ISTART,QFUEL);

            IF QCOST > 0 THEN
               IF QRESULT#(ISTART,QCOST) >= 10000 THEN
                  PRINT#1,USING "  $###.##";QRESULT#(ISTART,QCOST)-10000;
                  PRINT#1," LOW FUEL!";
               ELSE PRINT#1,USING "  $###.##";QRESULT#(ISTART,QCOST);
               ENDIF
            ENDIF

            PRINT#1,LF$
         ENDIF
22830 NEXT ISTART
      RETURN

22900 CLS:PRINT "Select databases":PRINT
      PRINT " 1 - Deselect all"
      I$ = STR$(RFILES%)
      FOR II = FFILE% TO FFILE%+IMAXFILES-1
         PRINT II;"- ";FN$(II);
         III=INSTR(2,I$,MID$(STR$(II),2))
         IF III > 0 THEN PRINT TAB(20);"  SELECTED" ELSE PRINT
      NEXT II
      PRINT FFILE%+IMAXFILES;"- Select all"
      PRINT
      INPUT"File Selection";II
      IF II = 0 AND RFILES% > 0 THEN RETURN
      IF II = 1 THEN RFILES%=0:GOTO 22900
      IF II = FFILE%+IMAXFILES THEN 22920
      III=INSTR(2,I$,MID$(STR$(II),2))
      IF III = 0 THEN RFILES%=RFILES%*10+II GOTO 22900
      IF III = 1 THEN RFILES%=VAL(MID$(I$,III+1)):GOTO 22900
      RFILES%=VAL(LEFT$(I$,III-1)+MID$(I$,III+1))
      GOTO 22900

22920 RFILES% = 0
      FOR II = FFILE% TO FFILE%+IMAXFILES-1
         RFILES% = RFILES% * 10 + II
      NEXT II
      GOTO 22900

23000 PRINT:PRINT"Sort by:"
      FOR I = 0 TO SF
      PRINT I+1;"-";SF$(I)
      NEXT I
      PRINT:INPUT"Selection";JJ
      JJ = JJ - 1
      IF JJ<0 OR JJ > SF THEN RETURN

      JJ = SF%(JJ)
      FOR J = 1 TO CRESULTS-1
         II = J
         FOR I = J + 1 TO CRESULTS
            IF QRESULT#(I,JJ) < QRESULT#(II,JJ) THEN II = I
         NEXT I
         IF II <> J THEN FOR III = 0 TO IMAXRESULTS:SWAP QRESULT#(J,III) , QRESULT#(II,III):NEXT III
      NEXT J
      RETURN

28300 REM Accept a new Identifier
      GOSUB 20160
      IF LEN(I$) = KEYSIZE(FILE%) THEN LSET ID$=II$:RETURN
      IF KEYSIZE(FILE%) = 4 AND LEN(I$) = 3 THEN LSET ID$=II$:RETURN
      PRINT CHR$(7);
      RETURN

28320 REM Accept a Name for the location into NAM$
      INPUT"Name        ";I$:IF I$ = "" THEN RETURN
      GOSUB UPPERCASE
      NAM$=II$
      IF LEN(II$) > 20 THEN PRINT CHR$(7);NAM$=LEFT$(NAM$,20)
      RETURN

28340 REM Accept a new City into CTY$
      INPUT"City";I$:IF I$ = "" THEN RETURN
      GOSUB UPPERCASE:CTY$=II$:IF LEN(II$) > 20 THEN PRINT CHR$(7);
      RETURN

28360 REM Accept a new State into ST$
      INPUT"State";I$:IF I$ = "" THEN RETURN
      GOSUB UPPERCASE
      IF LEN(II$) > 2 THEN PRINT CHR$(7);
      ST$=II$
      RETURN

28500 REM Accept a new Elevation into ELE$
      INPUT"Field elevation";I$
      IF I$ = "" THEN RETURN
      IF LEN(I$) > 6 OR VAL(I$) < -200 THEN PRINT I$;" doesn't make any sense":GOTO 28500
      ELE$=STR$(VAL(I$))
      RETURN

28600 REM Get a Reference VOR
      INPUT"Reference navaid";I$
      IF I$ <> "" THEN REF1$=I$
      RETURN

28620 REM Get a Reference Radial
      INPUT"Reference radial";I$
      IF I$ = "" THEN RETURN
      IF I$="0" THEN RADIAL=360
      IF VAL(I$) > 360 OR VAL(I$)<=0 THEN PRINT CHR$(7):RETURN
      RADIAL = VAL(I$)
      RETURN

28640 REM Get a Reference Distance
      INPUT"Reference distance";I$
      IF I$ = "" THEN RETURN
      DIST = VAL(I$)
      RETURN

40000 CLS:PRINT SPC(30);"Basic E6B Calculations"
      PRINT
      PRINT" 1 - Enter a new route"
      PRINT" 2 - Edit active route"
      PRINT" 3 - Save a route to disk"
      PRINT" 4 - Recall a route from disk"
      PRINT" 5 - Invert route"
      PRINT" 6 - Winds aloft"
      PRINT
      PRINT" 7 - Display/Print navigation calculations"
      PRINT
      PRINT" 8 - Configuration Defaults and Database Maintenance"
      PRINT
      PRINT" 9 - Database queries"

40200 LL=17
      LOCATE LL,5:PRINT SPC(70):LOCATE LL,5:INPUT"selection";I
      IF I < 0 OR I> 9 THEN PRINT"Error in selection":GOTO 40200
      PRINT SPC(70);CHR$(13)
      IF I=0 THEN INPUT"Verify exit";I$:IF I$="" OR LEFT$(I$,1)="Y" or LEFT$(I$,1) = "y" THEN SYSTEM ELSE GOTO 40000
      ON I GOSUB 2000,2005,2800,2900,4000,13000,10000,14900,22000
      GOTO 40000
