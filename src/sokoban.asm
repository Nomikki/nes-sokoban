; 	*****************************************
; 	***	SOKOBAN-KLOONI	by: Kimon Kukulis ***
; 	*****************************************
;	Yksinkertainen sokoban-klooni.
; 	Sokoban perustuu ideaan jossa työnnetään laatikoita tiettyihin kohtiin kentässä.
;	Kun kaikki laatikot on laitettu oikeisiin kohtiin päästään seuraavaan tasoon.
;
;	*****************************************

  .inesprg 1   ; 1x 16KB PRG code
  .ineschr 1   ; 1x  8KB CHR data
  .inesmap 0   ; mapper 0 = NROM, ei bank swappia
  .inesmir 1   ; taustan peilaus
  
;;;;;;;;;;;;;;;

;; Määritellään muuttujat täällä
  .rsset $0000  ;;start variables at ram location 0
  
gamestate    .rs 1  ; .rs 1 tarkoittaa että varataan 1 tavu 
playerx      .rs 1  ;
playery      .rs 1  ;
playerup     .rs 1  ;
playerdown   .rs 1  ;
playerleft   .rs 1  ;
playerright  .rs 1  ;
playerBtnA	 .rs 1  ;
playerBtnB   .rs 1  ;
playerBtnSelect .rs 1 ;
playerBtnStart  .rs 1 ;

buttons   	 .rs 1  ; pelaajan napit mitä on painettu, tästä kaavitaan arvot yllä oleviin muuttujiin
mapposX		 .rs 1
mapposY		 .rs 1
buttonCooldown .rs 1 

futureX		 .rs 1 ; sijainti johon koitetaan liikkua
futureY 	 .rs 1 


walkSound	 .rs 1
animaatio	 .rs 1
pointerLo    .rs 1   ; pointteri
pointerHi    .rs 1   ; 

boxPosX	     .rs 4 ; kaikkien laatikoiden x-arvot
boxPosY	     .rs 4 ; kaikkien laatikoiden y-arvot
boxPosSuunta .rs 4 ; mihin suuntaan liikutetaan laatikkoa
boxLukossa   .rs 4 ; Onko laatikko lukittu vai ei.
boxPosXtemp  .rs 4 ; Väliaikaiset muuttujat, jos laatikko esim törmää, asetetaan se edelliseen sijaintiin
boxPosYtemp  .rs 4 

boxStartPosX .rs 4 ; kaikkien laatikoiden aloitus-x-arvot
boxStartPosY .rs 4 ; kaikkien laatikoiden aloitus-y-arvot

mapPos 	     .rs 1
mapPosTemp 	 .rs 1
pitCounter	 .rs 1 ; Montako kuoppaa ruudulla
lockedCounter .rs 1 ; Montako laatikkoa on kuopassa 
lvl 		 .rs 256
currentLvl	 .rs 1 ; nykyinen kenttä
loopVal		 .rs 1
loopVal2	 .rs 1 
loopVal3	 .rs 1 
;; määritellään muutamat vakiot
STATETITLE     = $00  ; 
STATEPLAYING   = $01  ; 
STATEGAMEOVER  = $02  ; 
  
WALL     = $01
BOX      = $02
PIT    	 = $03




  
;;;;;;;;;;;;;;;;;;


  .bank 0
  .org $C000 
RESET:
  SEI          ; IRQ:t pois käytöstä
  CLD          ; ei käytetään desimaalitilaa
  LDX #$40
  STX $4017    ; APU pois käytöstä
  LDX #$FF
  TXS          ; Asetetaan pino
  INX          ; Nyt X = 0
  STX $2000    ; NMI pois päältä
  STX $2001    ; rendaus pois päältä
  STX $4010    ; IRQ DMC pois päältä

  
  ;Tehdään äänitesti
  lda #$01	; square 1
  sta $4015
  lda #$08	; period low
  sta $4002
  lda #$02	; period high
  sta $4003
  lda #$bf	; volume
  sta $4000
  
  
	JSR vblankwait       ; Odotellaan ensimmäistä vblankkia


clrmem:
	LDA #$00
	STA $0000, x
	STA $0100, x
	STA $0300, x
	STA $0400, x
	STA $0500, x
	STA $0600, x
	STA $0700, x
	LDA #$FE
	STA $0200, x
	INX
	BNE clrmem

	JSR vblankwait       ; Odotellaan seuraavaa vblankkia

	
	LDA #$01 
	STA currentLvl
	;JSR loadCurrentMap
  
;;:asetetaan pelitila
  LDA #STATETITLE
  STA gamestate

  ;tehdään ääni
  lda #$00	;
  sta $4015
  lda #$08	;
  sta $4002
  lda #$01	;
  sta $4003
  lda #$bf	;
  sta $4000
   
   
   ; JSR vblankwait       ; Odotellaan seuraavaa vblankkia
	JSR loadPalettes
	
	JSR loadTitleScreen
	
	LDA #%10010000   ;
	STA $2000

	LDA #%00011110   ; 
	STA $2001
	
Forever:
  JMP Forever
  

NMI:
  LDA #$00
  STA $2003       
  LDA #$02
  STA $4014       

  JSR DrawScore

  ;puhdistetaan ppu
  LDA #%10010000   ; otetaan nmi käyttöön, spritet pattern 0:sta, bg taulusta 1
  STA $2000
  LDA #%00011110   ; spritet, bg ja left-side clipping päälle
  STA $2001
  LDA #$00        ;ilmoitetaan ppu:lle, ettei ole skrollaava tausta
  STA $2005
  STA $2005
    
 
  ; Kaikki grafiikkapäivitykset tehty, pyöritetään pelimoottoria
  LDX buttonCooldown
  CPX #$00
  BNE hyppaaReadControllerinYli
  JSR ReadController  
hyppaaReadControllerinYli:

  
GameEngine:  
  LDA gamestate
  CMP #STATETITLE
  BEQ EngineTitle   
    
  LDA gamestate
  CMP #STATEGAMEOVER
  BEQ EngineGameOver
  
  LDA gamestate
  CMP #STATEPLAYING
  BEQ EnginePlaying 
GameEngineDone:  
  
  JSR UpdateSprites 

  RTI             ; Palataan keskeytykseen
 
 
;;;;;;;;
 
EngineTitle:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load game screen
  ;;  set starting paddle/ball position
  ;;  go to Playing State
  ;;  turn screen on
  
  LDA playerBtnA
  CMP #$01 
  BEQ aloita
  JMP GameEngineDone
 aloita:
  LDA #STATEPLAYING
  STA gamestate
  LDA #$01 
  STA currentLvl
  JSR loadCurrentMap
  
  JMP GameEngineDone

;;;;;;;;; 
 
EngineGameOver:
  ;;if start button pressed
  ;;  turn screen off
  ;;  load title screen
  ;;  go to Title State
  ;;  turn screen on 
  JMP GameEngineDone
 
;;;;;;;;;;;
 
EnginePlaying:
	
	; Jos painetaan A:ta, resetoidaan mappi 
	LDA playerBtnA
	CMP #$01 
	BEQ resetoiKartta
	jmp alaResetoi
resetoiKartta:

	JSR loadCurrentMap
	
alaResetoi:
	
	
    ; Haetaan buttoncooldown-arvo. Jos 0, ei tarvitse vähentää.
	LDX buttonCooldown
	CPX #$00
	BEQ hyppaacooldowninYli2
	DEX
	STX buttonCooldown
	
hyppaacooldowninYli2:



	JSR checkMovements
	
	
	
; Sitten laatikot kuntoon 
	LDX #$00 
	STX lockedCounter
	
	LDX #$00
tarkistaLootatSetupLoop:

	; Temppiin nykyinen sijainti 
	LDA boxPosX, X 
	STA boxPosXtemp, X 
	
	LDA boxPosY, X 
	STA boxPosYtemp, X 
	
	INX
	CPX #$04 
	BNE tarkistaLootatSetupLoop
	
	
	LDX #$00
tarkistaLootatLoop:
	
	; Tarkistetaan ensin onko laatikko lukossa vai ei.
	; Jos se on lukossa, hypätään tarkistusten yli 
	;LDA boxLukossa, X 
	;CMP #$01 
	;BEQ alaTuhoaLootaa ; laatikko on lukossa, jotenka hypätään tarkistusten yli 
	
	
	; Tarkistetaan suunta ja siirretään laatikkoa sen mukaan
	LDY boxPosSuunta, X
	CPY #$01 ;vasen
	BNE tarkistaLootatVasenSkip
	LDA boxPosX, X;
	SBC #$01 
	STA boxPosX, X;
tarkistaLootatVasenSkip:
	
	CPY #$02 ;oikea
	BNE tarkistaLootatOikeaSkip
	LDA boxPosX, X;
	ADC #$00 
	STA boxPosX, X;
tarkistaLootatOikeaSkip:
	
	CPY #$03 ;ylös
	BNE tarkistaLootatYlosSkip
	LDA boxPosY, X;
	SBC #$01 
	STA boxPosY, X;
tarkistaLootatYlosSkip:
	
	CPY #$04 ;alas
	BNE tarkistaLootatAlasSkip
	LDA boxPosY, X;
	ADC #$00 
	STA boxPosY, X;
tarkistaLootatAlasSkip:

	;lopuksi resetoidaan aina suunta
	LDA #$00 
	STA boxPosSuunta, X
	
	; tarkistetaan onko laatikko kuopassa.
	; jos on, siirretään se 0,0-kohtaan.
	; Ensin haetaan sijainti 1d-taulussa, eli 2d->1d
	LDA boxPosY, X; ;haetaan sijainti y-tasossa
	ROL A ;kerrotaan 16:lla.
	ROL A
	ROL A
	ROL A
	ADC boxPosX, X  ;lisätään X.
	;viedään arvo temp-muuttujaan
	STA mapPosTemp
	; Katsotaan mikä on mapPos-id tässä kohtaa karttaa
	LDY mapPosTemp
	LDA lvl, Y
	CMP #$00 ; jos 0, älä tuhoa laatikkoa vaan siirrä se.
	BEQ alaTuhoaLootaa ;hypätään siis seuraavien koodien yli

	; jos arvo on jotain muuta kuin 0, päädytään tänne.
	; ladatan arvo taas A-rekisteriin.
	LDA lvl, Y
	CMP #$01 ; jos arvo on kaikkea muuta kuin 1, eli seinä
	BNE poistaLoota
	
	;LDA boxStartPosX, X ;#$00 
	;STA boxPosX, X
	
	;LDA boxStartPosY, X ;#$00 
	;STA boxPosY, X
	
	; Asetetaan vanha sijainti 
	LDA boxPosXtemp, X 
	STA boxPosX, X
	LDA boxPosYtemp, X 
	STA boxPosY, X
	
	
	jmp alaTuhoaLootaa
	
poistaLoota:
	; Katsotaan onko laatikko lukossa, jos ei ole vähennetään pitcounteria yhdellä.
	; jos pitcounter on 0, siirrytään seuraavaan kenttään!
	;LDA boxLukossa, X 
	;CMP #$00 
	;BNE vahennus1;
	; laatikko ei ole lukossa
	
	LDA lockedCounter
	CLC
	ADC #$01
	STA lockedCounter
	;
vahennus1:
	
	; Asetetaan mapin sijainniksi ns. seinä, jotta muut ei voi viskoa asioita kuoppaan 
	;LDA #$01 
	;STA lvl, Y

	; Lopuksi lukitaan laatikko, koska se osui kuoppaan.
	; STA boxLukossa, X 
	
alaTuhoaLootaa:
	
	
	INX
	CPX #$04 
	BEQ tarkistaLootatLoopSkip
	JMP tarkistaLootatLoop
tarkistaLootatLoopSkip:



; Vielä tarkistus, jos sijainnissa on jo objekti, siirretään nykyinen objekti temp-sijaintiinsa
	LDX #$00 
	
	
objLoopOuter:
	LDY #$00 
objLoopInner:	
	
	LDA boxPosX, Y ; Ladataan sijainti-X A-rekisteriin 
	CMP boxPosX, X 
	BNE skipObjTest
	; Jos X-arvot täsmää, tarkistetaan vielä Y-arvot 
	LDA boxPosY, Y ; Ladataan sijainti-X A-rekisteriin 
	CMP boxPosY, X 
	BNE skipObjTest
	; Jos molemmat täsmäsi, tarkistetaan ettei y ja x ole samoja
	; TODO tänne
	STY loopVal ;joudutaan käyttää apumuuttujaa, koska vertailua rekisterien kesken ei voi tehdä
	CPX loopVal 
	BEQ skipObjTest ; koska arvot oli samat, hypätään yli
		
	; Jos päästiin tänne, eri objektit kyseessä ja samat koordinaatit 
	LDA boxPosXtemp, X
	STA boxPosX, X
	
	LDA boxPosYtemp, X
	STA boxPosY, X
	
	
skipObjTest:
		
	INY
	CPY #$04
	BNE objLoopInner

	INX
	CPX #$04	
	BNE objLoopOuter
	
	; Sitten tarkistetaan paljonko on pitcounter - 
	LDA pitCounter
	CLC	
	SBC lockedCounter
	CMP #$FF 
	BEQ loadNextLevel
	
	
  JMP GameEngineDone
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
loadNextLevel:
	LDA currentLvl
	ADC #$00 
	STA currentLvl
	JSR loadCurrentMap
	
	
	JMP GameEngineDone
  
  
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkMovements:

MovePlayerRight:
  LDA playerright
  BEQ MovePlayerRightDone   

  LDA playerx
  CLC
  ADC #$01        
  STA playerx
	LDA #$00 
	;STA playerright
	STA playerleft
	STA playerup
	STA playerdown

MovePlayerRightDone:

MovePlayerLeft:
  LDA playerleft
  BEQ MovePlayerLeftDone  

  LDA playerx
  SEC
  SBC #$01        
  STA playerx
    LDA #$00 
	STA playerright
	;STA playerleft
	STA playerup
	STA playerdown

MovePlayerLeftDone:


MovePlayerUp:
  LDA playerup
  BEQ MovePlayerUpDone  

  LDA playery
  SEC
  SBC #$01        
  STA playery
  LDA #$00 
	STA playerright
	STA playerleft
	;STA playerup
	STA playerdown

MovePlayerUpDone:


MovePlayerDown:
  LDA playerdown
  BEQ MovePlayerDownDone 

  LDA playery
  CLC
  ADC #$01         
  STA playery
  LDA #$00 
	STA playerright
	STA playerleft
	STA playerup
	;STA playerdown

MovePlayerDownDone:

	LDX buttonCooldown
	CPX #$00
	BNE hyppaacooldowninYli3
	JSR stopSound
hyppaacooldowninYli3:
	RTS
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

loadTitleScreen:
	
	lda #$20
	sta $2006 ; give $2006 both parts of address $2020.
	sta $2006 

	ldx #$00
loadNames:
	lda taustakuva, X ; load A with a byte from address (ourMap + X)
	inx
	sta $2007
	cpx #00 ; map in previous section 64 bytes long
	bne loadNames ; if not all 64 done, loop and do some more
	

	RTS
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
UpdateSprites:

	LDA gamestate
	CMP #STATEPLAYING
	BEQ drawGameSprites
	
	LDA gamestate
	CMP #STATETITLE
	BEQ drawTitle_start
	
	jmp drawDone
	
drawTitle_start:
	jmp drawTitle
	
drawGameSprites:
  LDA playery  ;
  STA $0200
  
  LDA #$32
  STA $0201
  
  LDA #$00
  STA $0202
  
  LDA playerx
  STA $0203
  
  
  
  LDA playery  ;
  STA $0204
  
  LDA #$33
  STA $0205
  
  LDA #$00
  STA $0206
  
  LDA playerx
  CLC
  adc #$08
  STA $0207
  
  
  LDX animaatio
  CPX #$01 
  BNE anim1
  
  LDA playery  ;
  CLC
  adc #$08
  STA $0208
  LDA #$42
  STA $0209
 
  LDA #$00
  STA $020A
  LDA playerx
  STA $020B
  
  
  LDA playery  ;
  CLC
  adc #$08
  STA $020C
  LDA #$43
  STA $020D
  
  LDA #$00
  STA $020E
  
  LDA playerx
  CLC
  adc #$08
  STA $020F
  
  jmp anim1end
  
 anim1:
  LDA playery  ;
  CLC
  adc #$08
  STA $0208
  LDA #$44
  STA $0209
 
  LDA #$00
  STA $020A
  LDA playerx
  STA $020B
  
  
  LDA playery  ;
  CLC
  adc #$08
  STA $020C
  LDA #$45
  STA $020D
  
  LDA #$00
  STA $020E
  
  LDA playerx
  CLC
  adc #$08
  STA $020F
  anim1end:
  
  
  ;;;;;;;;;;;;
  CLC
 
  ; Sitten pitäisi käydä kaikki laatikot läpi
  ; X pitää sisällään montako objektia on rendattu
  ; Y siirtää arvoa mihin osoitteeseen lähetetään tavaraa
  ; Eli joka kierroksella sen arvoa kasvatetaan neljällä.
  LDX #$00
  LDY #$00
boxRenderLoop:  
  LDA boxPosY, X ; Haetaan y-sijainti
  ASL A			 ; Kerrotaan se 16
  ASL A
  ASL A
  ASL A
  STA $0210, Y	 ; Asetetaan saatu arvo PPU:n puolelle
  
  LDA #$FA		; Tile joka rendataan
  STA $0211, Y
  
  LDA #$01		; Paletti
  STA $0212, Y
 
  LDA boxPosX, X ; Objektin x-arvo
  ASL A			
  ASL A
  ASL A
  ASL A
  STA $0213, Y	; Lähetetään X-sijainti PPU:lle.
  INY	; Kasvatetaan Y:tä neljä kertaa. 
  INY
  INY
  INY
  INX 
  CPX #$04	; Verrataan, onko luku neljä.
  BNE boxRenderLoop	; Jos ei ole, käydään looppia taas läpi.
  
	jmp drawDone
	

drawTitle:

	jmp drawDone
drawDone:
	
 
 
  RTS
 
 
DrawScore:
  ; rendataan pisteet
  
  RTS
 

; Haetaan sijainti mapDatasta, joka on muotoa: y * 16 + x
; ja tallennetaan tulos mapPos-muuttujaan 
haeCollPos:
	LDA mapposY; ;haetaan sijainti y-tasossa
	;CLC
	ROL A ;kerrotaan 16:lla.
	;CLC
	ROL A
	;CLC
	ROL A
	;CLC
	ROL A
	
	ADC mapposX  ;lisätään X.
	STA mapPos ; Tallennetaan sijainti.
	RTS
 
; Törmäystarkistus, jos menosuunnassa on seinä tai laatikko, pysähdytään
doColl:

	
	; Tarkistetaan seinät
	JSR haeCollPos

	LDA #$00 
	
	LDX mapPos
	DEX
	CLC
	LDY lvl, X
	CPY #$01 
	BNE doCollSkipLeft
	STA playerleft
	doCollSkipLeft:
	
	
	LDX mapPos
	INX
	CLC
	LDY lvl, X
	CPY #$01 
	BNE doCollSkipRight
	STA playerright
	
	doCollSkipRight:
	
	LDA mapPos
	sec
    sbc #$10
	STA mapPosTemp
	LDX mapPosTemp
	LDY lvl, X
	CPY #$01 
	BNE doCollSkipUp
	LDA #$00
	STA playerup
	
	doCollSkipUp:
	
	
	LDA mapPos
	clc
    adc #$10
	STA mapPosTemp
	LDX mapPosTemp
	LDY lvl, X
	CPY #$01 
	BNE doCollSkipDown
	LDA #$00
	STA playerdown
	
	doCollSkipDown:
	
	

	; Tarkistetaan ensin laatikot
	; Käydään silmukassa kaikki laatikot läpi 
	; jos laatikon koordinaatti on sama kuin pelaajahahmon koordinaatit + suunta
	; pysähdytään + asetetaan suunnaksi 0. Liikutetaan laatikkoa suunnan verran 

	;
	LDA mapposX
	STA futureX;
	LDA mapposY
	STA futureY;
	
	CLC
	
	LDX playerleft
	CPX #$01 
	BNE futLeftSkip
	LDA mapposX
	SEC
	SBC #$01 
	STA futureX;
	jmp futDownSkip
futLeftSkip:

	CLC
	
	LDX playerright
	CPX #$01 
	BNE futRightSkip
	LDA mapposX
	
	ADC #$00 ; mitä ihmettä, miksi 00 kasvattaa yhdellä? No toimii.
	CLC
	SEC
	STA futureX;
	jmp futDownSkip
futRightSkip:

	CLC
	
	LDX playerup
	CPX #$01 
	BNE futUpSkip
	LDA mapposY
	CLC
	SEC
	SBC #$01 
	STA futureY;
	jmp futDownSkip
futUpSkip:

	CLC
	
	LDX playerdown
	CPX #$01 
	BNE futDownSkip
	LDA mapposY
	;SEC
	CLC
	ADC #$01 
	STA futureY;
	
futDownSkip:
	
	
	LDX #$00
	CLC
	
doCollLoop:
	LDA #$00
	STA boxPosSuunta, X
	
	LDY boxPosX, X
	CPY futureX ;Verrataan ollaanko samassa sijainnissa x-tasolla
	BNE skipCollTest
	
	LDY boxPosY, X
	CPY futureY ;verrataan ollaanko samassa sijainnissa y-tasolla
	BNE skipCollTest
	; ollaan samassa sijainnissa nykyisen komponentin kanssa!
	
	; törmättiin! Siirretään nyt laatikkoa 
	LDA futureX
	STA boxPosX, X 
	
	LDA futureY
	STA boxPosY, X 
	
	
	LDY playerleft
	CPY #$01 
	BNE boxPosSuuntaVasenSkip
	LDA #$01
	STA boxPosSuunta, X
	jmp collFinalize
	
boxPosSuuntaVasenSkip:
	 
	LDY playerright
	CPY #$01 
	BNE boxPosSuuntaOikeaSkip
	LDA #$02
	STA boxPosSuunta, X
	jmp collFinalize
	
boxPosSuuntaOikeaSkip:

	LDY playerup
	CPY #$01 
	BNE boxPosSuuntaYlosSkip
	LDA #$03
	STA boxPosSuunta, X
	jmp collFinalize
	
boxPosSuuntaYlosSkip:
	
	LDY playerdown
	CPY #$01 
	BNE boxPosSuuntaAlasSkip
	LDA #$04
	STA boxPosSuunta, X
	jmp collFinalize
	
boxPosSuuntaAlasSkip:	

collFinalize:
	LDA #$00 
	STA buttonCooldown;
	STA playerleft
	STA playerup
	STA playerright
	STA playerdown 
	jmp collFinalize2
	
skipCollTest:
	
	INX 
	CPX #$04
	BNE doCollLoop
	
	LDX #$00
	
collFinalize2:

	RTS
	
 
 
; Luetaan mitä nappeja painetaan
ReadController:


	  ; Strobetetaan kontrolleria
	  LDA #$01
	  STA $4016
	  LDA #$00
	  STA $4016
	  LDX #$08
	ReadControllerLoop:
	  LDA $4016 ;Luetaan syöte
	  LSR A           ; bit0 -> Carry
	  ROL buttons     ; bit0 <- Carry
	  DEX
	  BNE ReadControllerLoop
	 
	LDX buttons
	CPX #$00
	BEQ hyppaacooldowninYli
	JSR playSound
	LDA #$10
	STA buttonCooldown
    
hyppaacooldowninYli:

	LDA #$00
	STA playerright
	STA playerleft
	STA playerup
	STA playerdown
	
	STA playerBtnA
	STA playerBtnB
	STA playerBtnSelect
	STA playerBtnStart
	
	

	LDX buttons
	CPX #$00
  
	LDA buttons
	LSR A
	ROL playerright
	LSR A
	ROL playerleft
	LSR A
	ROL playerdown
	LSR A
	ROL playerup
	
	LSR A
	ROL playerBtnStart
	LSR A
	ROL playerBtnSelect
	LSR A
	ROL playerBtnB
	LSR A
	ROL playerBtnA
	
  

	
		; Tarkistetaan hahmon sijainti kertalla (eli jaetaan 16:lla)
	LDA playerx 
	LSR A
	LSR A
	LSR A
	LSR A
	STA mapposX
	
	LDA playery 
	LSR A
	LSR A
	LSR A
	LSR A
	STA mapposY

	
	JSR doColl
	
	
  RTS
  


;;;;;;;;;;;;;
playSound:

  
  lda #$01	; square 1
  sta $4015
  
  LDX walkSound
  CPX #$00
  BEQ KorkeaAani
  
 MatalaAani: 
  LDA #$00 
  STA animaatio
  
  DEX
  lda #$08	; period low
  sta $4002
  lda walkSound	; period high
  sta $4003
  lda #$bf	; volume
  sta $4000
  lda #$02	; 
  sta $4000
  STX walkSound
   
  RTS
 
 KorkeaAani:
  LDA #$01 
  STA animaatio

  
  INX 
  lda #$08	; period low
  sta $4002
  lda #$02	; period high
  sta $4003
  lda #$bf	; volume
  sta $4000
  lda #$02	; 
  sta $4000
  
  STX walkSound
  
  RTS  

stopSound:
  lda #$00	; square 1
  sta $4015
  RTS
 
;;;;;;;;;;;;;;  
 
vblankwait:
	BIT $2002
	BPL vblankwait
  
	RTS
 
loadCurrentMap:

	;Kun ladataan karttaa, joudumme päivittää taustakuvan, jonka vuoksi otetaan väliaikaisesti
	;NMI pois päältä (ei voida rendata kesken lataamisen joka sekoittaa pakkaa)
	;ja vielä kielletään rendauskin
	LDX #$00
	STX $2000    ; NMI pois päältä
	STX $2001    ; rendaus pois päältä
  
	;;;;;;;;;;;;;;;;;;
	; Ladataan kartta ROM-muistista RAM:n puolelle, jotta voidaan tehdä muutoksia ja aina kaikki tarkistukset
	; tehdään tähän.
	JSR vblankwait       ; Odotellaan seuraavaa vblankkia
  
	JSR loadPalettes
	JSR loadMap 
	JSR loadBackground
	JSR calcPitCounter
	
	; alustetaan arvoja
	LDA #$00
	STA playerdown
	STA playerright
	STA playerleft
	STA playerup
	STA animaatio
 
	JSR findPlayerStartPosition
	JSR setBoxes

	;Lopuksi asetetaan NMI ja rendaukset päälle, koska nyt on turvallista taas.
	LDA #%10010000   ;
	STA $2000

	LDA #%00011110   ; 
	STA $2001
	RTS
 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Ladataan paletti uudestaan, jotta menee oikeaan paikkaan
loadPalettes:
  LDA $2002             ; Luetaan PPU:n tila resetoidaksemme se (perus strobe)
  LDA #$3F
  STA $2006             ; Kirjoita 0x3fxx
  LDA #$00
  STA $2006             ; kirjoita 0xXX00
  LDX #$00              ; Aloitetaan nollasta
LoadPalettesLoop:
  LDA palette, x        ;Ladataan paletti + x rekisteriin A
  STA $2007             ; Kirjoitetaan PPU:lle
  INX                   ; X = X + 1
  CPX #$20              ; Verrataan. Jos luku on 0x20 (32d)
  BNE LoadPalettesLoop  ; Jos ei ole 0x20, jatketaan looppia.
	RTS
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 	; ladataan karttadata, eli mihin tulee esteet, missä on tilaa liikkua, missä on laatikot jne.
	; Tätä tietoa tarvitaan jotta saadaan komponentit paikoilleen
loadMap:
	LDX #$00
loadMapLoop:
	LDY currentLvl
	CPY #$01 
	BEQ ldLvl1
	CPY #$02 
	BEQ ldLvl2
ldLvl1:	
	LDA lvlData1, X
	jmp ldLvlDone
ldLvl2:	
	LDA lvlData2, X
	jmp ldLvlDone
ldLvlDone:
	STA lvl, X
	INX 
	CPX #$00
	BNE loadMapLoop
	jmp mapLoadDone
	
mapLoadDone:
	RTS
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
; Ladataan itse taustakuva myös 
loadBackground:
  LDA $2002             ; Resetoidaan PPU:n tila
  LDA #$20
  STA $2006             ; 
  LDA #$00
  STA $2006             ;

	LDA #$00
	STA pointerLo         ; Asetetaan pointterin matalat bitit pointerLo-muuttujaan
	STA pointerHi
	LDY currentLvl
	CPY #$01 
	BEQ ldLvl1_b
	CPY #$02 
	BEQ ldLvl2_b
	
ldLvl1_b:
	LDA #HIGH(background1) ;Ladataan background-osoitteen korkeammat bitit LDA:han
	jmp ldLvl_done
ldLvl2_b:	
	LDA #HIGH(background2) ;Ladataan background-osoitteen korkeammat bitit LDA:han
	jmp ldLvl_done
ldLvl_done:

	STA pointerHi         ;Asetetaan bitit pointerHi-muuttujaan
	LDX #$00            ; Aloitetaan: pointer + 0
	LDY #$00
OutsideLoop:
  
InsideLoop:
	LDA [pointerLo], y  ; Kopioidaan yksi background-tavu background-osoitteesta + y
	STA $2007           ; Tämä tunkee 256 tavua kerralla
  
	INY                 ; Kasvatetaan y:tä yhdellä.
	CPY #$00
	BNE InsideLoop      ; Annetaan pyörähtää ympäri. Kun arvo on 0 (sama kuin 256) jatketaan, muussa tapauksessa hypätään insideLoopiin.
  
	INC pointerHi       ; Kasvatetaan pointerHi:n verran.
  
	INX	; Kasvatetaan x:ää yhdellä.
	CPX #$04 ; Katsotaan onko 4, koska 256*4 = 1024
	BNE OutsideLoop     ; Jos ei ole, mennään outsideLoopiin.
	RTS
		
 ;;;;;;;;;;;;;;;;;;;;;;;;;;
 calcPitCounter:
	; Lasketaan montako kuoppaa
	LDA #$00
	LDX #$00
	; Asetetaan pitcounter-arvoksi 0 
	STA pitCounter
pitCounterLoop:
	LDY lvl, X 
	CPY #$02
	BNE skipPitCounterInc
	ADC #$00
	STA pitCounter
skipPitCounterInc:
	INX 
	CPX #$00 
	BNE pitCounterLoop
	RTS
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
findPlayerStartPosition:

  ; Etsitään mapista luku 4, se ilmaisee aloituskoordinaattia
	LDX #$00
initPlayerPos:
  
	LDY lvl, X
	CPY #$04
	BNE skipPlayerPos
	
	LDA #$00 
	STA lvl, X
	
	TXA ;siirretään x-rekisteri a-rekisteriin 
	
	lsr A
	lsr A
	lsr A
	lsr A
	
	STA playery
	
	asl A
	asl A
	asl A
	asl A
	
	STA loopVal3
	 
	TXA
	;CLC
	ADC #$01
	SBC loopVal3 ;A:sta vähennetään loopVal3
	
	;asetetaan A:han tallennettu y-sijainti
	STA playerx
	
 skipPlayerPos:
  
  
  INX
  CPX #$00
  BNE initPlayerPos
 

 
	; lopuksi kerrotaan playerx ja playery 16:lla.
	LDA playerx 
	asl A ;kerrotaan 16:lla.
	asl A
	asl A
	asl A
	STA playerx
 
	LDA playery 
	asl A ;kerrotaan 16:lla.
	asl A
	asl A
	asl A
	STA playery
	RTS

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 setBoxes:
 
 ; Asetetaan laatikot kentälle
 ; Ensin arvoiksi 0 
	LDA #$00 
	LDX #$00
initBoxesLoop:
	
	STA boxPosX, X
	STA boxPosY, X
	INX
	CPX #$04 
	BNE initBoxesLoop
	
	;Apumuuttujaan myös arvoksi 0
	STA loopVal
	
	; Käydään nyt koko kartta läpi
	LDA #$00
	LDX #$00
	
	
	
	LDA #$00
	LDX #$00
	LDY #$00
	
	
objCounterLoop:
	LDY lvl, X 
	CPY #$03
	BNE skipObjCounterInc
	
	
	
skipObjCounterInc:


	LDY lvl, X
	CPY #$03
	BNE skipPosSet ;luku ei ollut 3, voidaan hypätä yli siis
	; Asetetaan objektille sijainti 
	LDY loopVal
	
	; Haetaan mikä on tietyn id:n y-koordinaatti 
	STX loopVal2
	LDA loopVal2
	; Jaetaan luku 16:lla, jotta saadaan tietoon y-akseli
	lsr A
	lsr A
	lsr A
	lsr A
	;asetetaan A:han tallennettu x-sijainti
	STA boxPosY, Y
	
	;Nyt kasvatetaan samaa lukua 16-kertaiseksi
	asl A
	asl A
	asl A
	asl A
	; Asetetaan luku muistiin 
	STA loopVal3 
	; Ja vähennetään 
	LDA loopVal2
	ADC #$01
	SBC loopVal3
	
	;asetetaan A:han tallennettu y-sijainti
	STA boxPosX, Y
	
	;lopuksi asetetaan kenttädataan arvoksi 0 = ei estettä
	LDA #$00 
	STA lvl, X
	
	INY 
	STY loopVal
	
skipPosSet:

	INX
	CPX #$00 
	BNE objCounterLoop
 
	; Ladatan arvot aloitusarvoiksi:
	LDY #$00 
aloitusarvoLoop:
	LDA boxPosX, Y
	STA boxStartPosX, Y
	LDA boxPosY, Y
	STA boxStartPosY, Y
	
	; Ja asetetan ettei laatikko ole lukossa 
	LDA #$00 
	STA boxLukossa, Y
	
	INY 
	CPY #$04
	BNE aloitusarvoLoop
	RTS
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;	SUPER SOKOBAN 3000!!!

  .bank 1
 
;dummy 
;  org $E800
  ; Asetetaan alkamaan bank 1:n alusta, koska osoite on e000, voidaan jättää xx00 huomiotta, helpottaa datan lataamisen kanssa.
;background1:
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;  .db $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  ;
;
;
;attributes1:  ;8 x 8 = 64 bytes
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;  .db %11111111, %11111111, %11111111, %11111111
;
;
;  .db $24,$24,$24,$24, $47,$47,$24,$24, $47,$47,$47,$47, $47,$47,$24,$24, $24,$24,$24,$24 ,$24,$24,$24,$24, $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms
;  .db $47,$47,$47,$47, $47,$47,$24,$24 
;  .db $24,$24,$24,$24 ,$24,$24,$24,$24
;  .db $24,$24,$24,$24, $55,$56,$24,$24 

  
  
  
  
  .org $E000 
							;  1c  1e  19  0e  1b  24  1c  18  14  18   0b  0a  17  03  00  00  00  2b  2b  2b
 taustakuva:				;   S   U   P   E   R   	S   O   K   O    B   A   N   3   0   0   0   !   !   !
							;
							;   P   R   E   S   S       A 
							;  19  1b  0e  1c  1c  24  0a
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$1c,$1e,$19,$0e,$1b,$24,$1c,$18,$14,$18, $0b,$0a,$17,$03,$00,$00,$00,$2b,$2b,$2b,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$19,$1b,$0e,$1c,$1c,$24,$0a,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
 

attributes_title:  ;8 x 8 = 64 bytes
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111


  
  
  .org $E800
  ; Asetetaan alkamaan bank 1:n alusta, koska osoite on e000, voidaan jättää xx00 huomiotta, helpottaa datan lataamisen kanssa.
background1:
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$27,$27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $27,$27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$27,$27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $27,$27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $27,$27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $27,$27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$27,$27,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$27,$27,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;


attributes1:  ;8 x 8 = 64 bytes
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111


  .db $24,$24,$24,$24, $47,$47,$24,$24, $47,$47,$47,$47, $47,$47,$24,$24, $24,$24,$24,$24 ,$24,$24,$24,$24, $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms
  .db $47,$47,$47,$47, $47,$47,$24,$24 
  .db $24,$24,$24,$24 ,$24,$24,$24,$24
  .db $24,$24,$24,$24, $55,$56,$24,$24 

	.org $F000
  
 background2:
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$27,$27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$27,$27,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$27,$27,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$27,$27,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$47,$47,$24,$24,$47,$47,$47,$47,$47,$47,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$55,$56,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$24,$24,$45,$45,$24,$24,$45,$45,$45,$45,$45,$45,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$53,$54,$45,$45  ;
  .db $45,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24, $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$45,$45  ;
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;
  .db $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45, $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45  ;


attributes2:  ;8 x 8 = 64 bytes
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111
  .db %11111111, %11111111, %11111111, %11111111


  .db $24,$24,$24,$24, $47,$47,$24,$24, $47,$47,$47,$47, $47,$47,$24,$24, $24,$24,$24,$24 ,$24,$24,$24,$24, $24,$24,$24,$24, $55,$56,$24,$24  ;;brick bottoms
  .db $47,$47,$47,$47, $47,$47,$24,$24 
  .db $24,$24,$24,$24 ,$24,$24,$24,$24
  .db $24,$24,$24,$24, $55,$56,$24,$24 
  
  
  
  
 palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .db $80, $32, $00, $80   ;sprite 0
  .db $80, $33, $00, $88   ;sprite 1
  .db $88, $34, $00, $80   ;sprite 2
  .db $88, $35, $00, $88   ;sprite 3
  
  

  ; Mappidatamme.
; 01 = seinä
; 02 = laatikko
; 03 = kuoppa johon laatikko laitetaan
; huom. 0.0-sijainnissa aina oltava 0-arvo, koska sinne menee siirretyt laatikot 
; Siirtäisin ennemmin mapin ulkopuolelle, mutta NES ei tunne etumerkillisiä arvoja, jotenka näin...
lvlData1:
	db $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $01, $01
	db $01, $00, $02, $00, $00, $03, $00, $00, $02, $00, $03, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $02, $00, $03, $00, $00, $00, $01, $01
	db $01, $00, $00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $02, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $01, $01
	db $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $01, $01
	db $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $01, $01
	db $01, $00, $00, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $01, $01
	db $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $01, $01
	db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
	db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  

lvlData2:
	db $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $01, $00, $00, $00, $00, $01, $01
	db $01, $00, $02, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $01, $00, $00, $00, $00, $01, $01
	db $01, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $02, $01, $01, $01, $00, $00, $01, $00, $00, $00, $00, $01, $01
	db $01, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $01, $00, $00, $00, $04, $01, $01
	db $01, $00, $00, $00, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $01, $00, $00, $00, $00, $01, $01
	db $01, $00, $00, $03, $00, $00, $00, $00, $00, $01, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $01, $01
	db $01, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $01
	db $01, $00, $01, $00, $01, $01, $01, $00, $00, $00, $00, $00, $00, $00, $01, $01
	db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
	db $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01
  
;dummylvl
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
	db $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  

  
  .org $FFFA     ;Määritellään sijainti josta seuraavat vektorit alkaa
  .dw NMI        ;Kun NMI tapahtuu, hypätään NMI-labeliin
  .dw RESET      ; Kun resetoidaan tai hypätään resettiin, hypätään RESET-osioon
  .dw 0          ; Ulkoiset irq:t ei ole käytössä, viitataan siis nollaan.
  
;;;;;;;;;;;;;;  
  
  
  .bank 2
  .org $0000
 ourMap:
  .incbin "testi1/mario.chr" 