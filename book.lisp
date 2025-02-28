;;; book.lisp --- routines for decoding polyglot blobs and pgn game lists

(in-package :leszcz)

;; default book from komodo
(defparameter *book-data* (file->vec "res/book/komodo.bin"))
(defparameter *book* (make-hash-table))

(loop for i from 0 below (length *book-data*) by 16 do
  (let* ((key (logior
               (ash (aref *book-data* i) 56)
               (ash (aref *book-data* (+ i 1)) 48)
               (ash (aref *book-data* (+ i 2)) 40)
               (ash (aref *book-data* (+ i 3)) 32)
               (ash (aref *book-data* (+ i 4)) 24)
               (ash (aref *book-data* (+ i 5)) 16)
               (ash (aref *book-data* (+ i 6)) 8)
               (aref *book-data*      (+ i 7))))
         (u16 (logior
               (ash (aref *book-data* (+ i 8)) 8)
               (aref *book-data* (+ i 9))))
         (x2 (logand #b111 u16))
         (y2 (- 7 (ash (logand #b111000 u16) -3)))
         (x1 (ash (logand #b111000000 u16) -6))
         (y1 (- 7 (ash (logand #b111000000000 u16) -9)))
         (promo (case (ash (logand #b111000000000000 u16) -12)
                  (0 nil)
                  (1 'knight)
                  (2 'bishop)
                  (3 'rook)
                  (4 'queen))))
    (setf (gethash key *book*) (list (list (list x1 y1) (list x2 y2) promo)))))

(defparameter *random-table*
  #(#x9D39247E33776D41 #x2AF7398005AAA5C7 #x44DB015024623547 #x9C15F73E62A76AE2
    #x75834465489C0C89 #x3290AC3A203001BF #x0FBBAD1F61042279 #xE83A908FF2FB60CA
    #x0D7E765D58755C10 #x1A083822CEAFE02D #x9605D5F0E25EC3B0 #xD021FF5CD13A2ED5
    #x40BDF15D4A672E32 #x011355146FD56395 #x5DB4832046F3D9E5 #x239F8B2D7FF719CC
    #x05D1A1AE85B49AA1 #x679F848F6E8FC971 #x7449BBFF801FED0B #x7D11CDB1C3B7ADF0
    #x82C7709E781EB7CC #xF3218F1C9510786C #x331478F3AF51BBE6 #x4BB38DE5E7219443
    #xAA649C6EBCFD50FC #x8DBD98A352AFD40B #x87D2074B81D79217 #x19F3C751D3E92AE1
    #xB4AB30F062B19ABF #x7B0500AC42047AC4 #xC9452CA81A09D85D #x24AA6C514DA27500
    #x4C9F34427501B447 #x14A68FD73C910841 #xA71B9B83461CBD93 #x03488B95B0F1850F
    #x637B2B34FF93C040 #x09D1BC9A3DD90A94 #x3575668334A1DD3B #x735E2B97A4C45A23
    #x18727070F1BD400B #x1FCBACD259BF02E7 #xD310A7C2CE9B6555 #xBF983FE0FE5D8244
    #x9F74D14F7454A824 #x51EBDC4AB9BA3035 #x5C82C505DB9AB0FA #xFCF7FE8A3430B241
    #x3253A729B9BA3DDE #x8C74C368081B3075 #xB9BC6C87167C33E7 #x7EF48F2B83024E20
    #x11D505D4C351BD7F #x6568FCA92C76A243 #x4DE0B0F40F32A7B8 #x96D693460CC37E5D
    #x42E240CB63689F2F #x6D2BDCDAE2919661 #x42880B0236E4D951 #x5F0F4A5898171BB6
    #x39F890F579F92F88 #x93C5B5F47356388B #x63DC359D8D231B78 #xEC16CA8AEA98AD76
    #x5355F900C2A82DC7 #x07FB9F855A997142 #x5093417AA8A7ED5E #x7BCBC38DA25A7F3C
    #x19FC8A768CF4B6D4 #x637A7780DECFC0D9 #x8249A47AEE0E41F7 #x79AD695501E7D1E8
    #x14ACBAF4777D5776 #xF145B6BECCDEA195 #xDABF2AC8201752FC #x24C3C94DF9C8D3F6
    #xBB6E2924F03912EA #x0CE26C0B95C980D9 #xA49CD132BFBF7CC4 #xE99D662AF4243939
    #x27E6AD7891165C3F #x8535F040B9744FF1 #x54B3F4FA5F40D873 #x72B12C32127FED2B
    #xEE954D3C7B411F47 #x9A85AC909A24EAA1 #x70AC4CD9F04F21F5 #xF9B89D3E99A075C2
    #x87B3E2B2B5C907B1 #xA366E5B8C54F48B8 #xAE4A9346CC3F7CF2 #x1920C04D47267BBD
    #x87BF02C6B49E2AE9 #x092237AC237F3859 #xFF07F64EF8ED14D0 #x8DE8DCA9F03CC54E
    #x9C1633264DB49C89 #xB3F22C3D0B0B38ED #x390E5FB44D01144B #x5BFEA5B4712768E9
    #x1E1032911FA78984 #x9A74ACB964E78CB3 #x4F80F7A035DAFB04 #x6304D09A0B3738C4
    #x2171E64683023A08 #x5B9B63EB9CEFF80C #x506AACF489889342 #x1881AFC9A3A701D6
    #x6503080440750644 #xDFD395339CDBF4A7 #xEF927DBCF00C20F2 #x7B32F7D1E03680EC
    #xB9FD7620E7316243 #x05A7E8A57DB91B77 #xB5889C6E15630A75 #x4A750A09CE9573F7
    #xCF464CEC899A2F8A #xF538639CE705B824 #x3C79A0FF5580EF7F #xEDE6C87F8477609D
    #x799E81F05BC93F31 #x86536B8CF3428A8C #x97D7374C60087B73 #xA246637CFF328532
    #x043FCAE60CC0EBA0 #x920E449535DD359E #x70EB093B15B290CC #x73A1921916591CBD
    #x56436C9FE1A1AA8D #xEFAC4B70633B8F81 #xBB215798D45DF7AF #x45F20042F24F1768
    #x930F80F4E8EB7462 #xFF6712FFCFD75EA1 #xAE623FD67468AA70 #xDD2C5BC84BC8D8FC
    #x7EED120D54CF2DD9 #x22FE545401165F1C #xC91800E98FB99929 #x808BD68E6AC10365
    #xDEC468145B7605F6 #x1BEDE3A3AEF53302 #x43539603D6C55602 #xAA969B5C691CCB7A
    #xA87832D392EFEE56 #x65942C7B3C7E11AE #xDED2D633CAD004F6 #x21F08570F420E565
    #xB415938D7DA94E3C #x91B859E59ECB6350 #x10CFF333E0ED804A #x28AED140BE0BB7DD
    #xC5CC1D89724FA456 #x5648F680F11A2741 #x2D255069F0B7DAB3 #x9BC5A38EF729ABD4
    #xEF2F054308F6A2BC #xAF2042F5CC5C2858 #x480412BAB7F5BE2A #xAEF3AF4A563DFE43
    #x19AFE59AE451497F #x52593803DFF1E840 #xF4F076E65F2CE6F0 #x11379625747D5AF3
    #xBCE5D2248682C115 #x9DA4243DE836994F #x066F70B33FE09017 #x4DC4DE189B671A1C
    #x51039AB7712457C3 #xC07A3F80C31FB4B4 #xB46EE9C5E64A6E7C #xB3819A42ABE61C87
    #x21A007933A522A20 #x2DF16F761598AA4F #x763C4A1371B368FD #xF793C46702E086A0
    #xD7288E012AEB8D31 #xDE336A2A4BC1C44B #x0BF692B38D079F23 #x2C604A7A177326B3
    #x4850E73E03EB6064 #xCFC447F1E53C8E1B #xB05CA3F564268D99 #x9AE182C8BC9474E8
    #xA4FC4BD4FC5558CA #xE755178D58FC4E76 #x69B97DB1A4C03DFE #xF9B5B7C4ACC67C96
    #xFC6A82D64B8655FB #x9C684CB6C4D24417 #x8EC97D2917456ED0 #x6703DF9D2924E97E
    #xC547F57E42A7444E #x78E37644E7CAD29E #xFE9A44E9362F05FA #x08BD35CC38336615
    #x9315E5EB3A129ACE #x94061B871E04DF75 #xDF1D9F9D784BA010 #x3BBA57B68871B59D
    #xD2B7ADEEDED1F73F #xF7A255D83BC373F8 #xD7F4F2448C0CEB81 #xD95BE88CD210FFA7
    #x336F52F8FF4728E7 #xA74049DAC312AC71 #xA2F61BB6E437FDB5 #x4F2A5CB07F6A35B3
    #x87D380BDA5BF7859 #x16B9F7E06C453A21 #x7BA2484C8A0FD54E #xF3A678CAD9A2E38C
    #x39B0BF7DDE437BA2 #xFCAF55C1BF8A4424 #x18FCF680573FA594 #x4C0563B89F495AC3
    #x40E087931A00930D #x8CFFA9412EB642C1 #x68CA39053261169F #x7A1EE967D27579E2
    #x9D1D60E5076F5B6F #x3810E399B6F65BA2 #x32095B6D4AB5F9B1 #x35CAB62109DD038A
    #xA90B24499FCFAFB1 #x77A225A07CC2C6BD #x513E5E634C70E331 #x4361C0CA3F692F12
    #xD941ACA44B20A45B #x528F7C8602C5807B #x52AB92BEB9613989 #x9D1DFA2EFC557F73
    #x722FF175F572C348 #x1D1260A51107FE97 #x7A249A57EC0C9BA2 #x04208FE9E8F7F2D6
    #x5A110C6058B920A0 #x0CD9A497658A5698 #x56FD23C8F9715A4C #x284C847B9D887AAE
    #x04FEABFBBDB619CB #x742E1E651C60BA83 #x9A9632E65904AD3C #x881B82A13B51B9E2
    #x506E6744CD974924 #xB0183DB56FFC6A79 #x0ED9B915C66ED37E #x5E11E86D5873D484
    #xF678647E3519AC6E #x1B85D488D0F20CC5 #xDAB9FE6525D89021 #x0D151D86ADB73615
    #xA865A54EDCC0F019 #x93C42566AEF98FFB #x99E7AFEABE000731 #x48CBFF086DDF285A
    #x7F9B6AF1EBF78BAF #x58627E1A149BBA21 #x2CD16E2ABD791E33 #xD363EFF5F0977996
    #x0CE2A38C344A6EED #x1A804AADB9CFA741 #x907F30421D78C5DE #x501F65EDB3034D07
    #x37624AE5A48FA6E9 #x957BAF61700CFF4E #x3A6C27934E31188A #xD49503536ABCA345
    #x088E049589C432E0 #xF943AEE7FEBF21B8 #x6C3B8E3E336139D3 #x364F6FFA464EE52E
    #xD60F6DCEDC314222 #x56963B0DCA418FC0 #x16F50EDF91E513AF #xEF1955914B609F93
    #x565601C0364E3228 #xECB53939887E8175 #xBAC7A9A18531294B #xB344C470397BBA52
    #x65D34954DAF3CEBD #xB4B81B3FA97511E2 #xB422061193D6F6A7 #x071582401C38434D
    #x7A13F18BBEDC4FF5 #xBC4097B116C524D2 #x59B97885E2F2EA28 #x99170A5DC3115544
    #x6F423357E7C6A9F9 #x325928EE6E6F8794 #xD0E4366228B03343 #x565C31F7DE89EA27
    #x30F5611484119414 #xD873DB391292ED4F #x7BD94E1D8E17DEBC #xC7D9F16864A76E94
    #x947AE053EE56E63C #xC8C93882F9475F5F #x3A9BF55BA91F81CA #xD9A11FBB3D9808E4
    #x0FD22063EDC29FCA #xB3F256D8ACA0B0B9 #xB03031A8B4516E84 #x35DD37D5871448AF
    #xE9F6082B05542E4E #xEBFAFA33D7254B59 #x9255ABB50D532280 #xB9AB4CE57F2D34F3
    #x693501D628297551 #xC62C58F97DD949BF #xCD454F8F19C5126A #xBBE83F4ECC2BDECB
    #xDC842B7E2819E230 #xBA89142E007503B8 #xA3BC941D0A5061CB #xE9F6760E32CD8021
    #x09C7E552BC76492F #x852F54934DA55CC9 #x8107FCCF064FCF56 #x098954D51FFF6580
    #x23B70EDB1955C4BF #xC330DE426430F69D #x4715ED43E8A45C0A #xA8D7E4DAB780A08D
    #x0572B974F03CE0BB #xB57D2E985E1419C7 #xE8D9ECBE2CF3D73F #x2FE4B17170E59750
    #x11317BA87905E790 #x7FBF21EC8A1F45EC #x1725CABFCB045B00 #x964E915CD5E2B207
    #x3E2B8BCBF016D66D #xBE7444E39328A0AC #xF85B2B4FBCDE44B7 #x49353FEA39BA63B1
    #x1DD01AAFCD53486A #x1FCA8A92FD719F85 #xFC7C95D827357AFA #x18A6A990C8B35EBD
    #xCCCB7005C6B9C28D #x3BDBB92C43B17F26 #xAA70B5B4F89695A2 #xE94C39A54A98307F
    #xB7A0B174CFF6F36E #xD4DBA84729AF48AD #x2E18BC1AD9704A68 #x2DE0966DAF2F8B1C
    #xB9C11D5B1E43A07E #x64972D68DEE33360 #x94628D38D0C20584 #xDBC0D2B6AB90A559
    #xD2733C4335C6A72F #x7E75D99D94A70F4D #x6CED1983376FA72B #x97FCAACBF030BC24
    #x7B77497B32503B12 #x8547EDDFB81CCB94 #x79999CDFF70902CB #xCFFE1939438E9B24
    #x829626E3892D95D7 #x92FAE24291F2B3F1 #x63E22C147B9C3403 #xC678B6D860284A1C
    #x5873888850659AE7 #x0981DCD296A8736D #x9F65789A6509A440 #x9FF38FED72E9052F
    #xE479EE5B9930578C #xE7F28ECD2D49EECD #x56C074A581EA17FE #x5544F7D774B14AEF
    #x7B3F0195FC6F290F #x12153635B2C0CF57 #x7F5126DBBA5E0CA7 #x7A76956C3EAFB413
    #x3D5774A11D31AB39 #x8A1B083821F40CB4 #x7B4A38E32537DF62 #x950113646D1D6E03
    #x4DA8979A0041E8A9 #x3BC36E078F7515D7 #x5D0A12F27AD310D1 #x7F9D1A2E1EBE1327
    #xDA3A361B1C5157B1 #xDCDD7D20903D0C25 #x36833336D068F707 #xCE68341F79893389
    #xAB9090168DD05F34 #x43954B3252DC25E5 #xB438C2B67F98E5E9 #x10DCD78E3851A492
    #xDBC27AB5447822BF #x9B3CDB65F82CA382 #xB67B7896167B4C84 #xBFCED1B0048EAC50
    #xA9119B60369FFEBD #x1FFF7AC80904BF45 #xAC12FB171817EEE7 #xAF08DA9177DDA93D
    #x1B0CAB936E65C744 #xB559EB1D04E5E932 #xC37B45B3F8D6F2BA #xC3A9DC228CAAC9E9
    #xF3B8B6675A6507FF #x9FC477DE4ED681DA #x67378D8ECCEF96CB #x6DD856D94D259236
    #xA319CE15B0B4DB31 #x073973751F12DD5E #x8A8E849EB32781A5 #xE1925C71285279F5
    #x74C04BF1790C0EFE #x4DDA48153C94938A #x9D266D6A1CC0542C #x7440FB816508C4FE
    #x13328503DF48229F #xD6BF7BAEE43CAC40 #x4838D65F6EF6748F #x1E152328F3318DEA
    #x8F8419A348F296BF #x72C8834A5957B511 #xD7A023A73260B45C #x94EBC8ABCFB56DAE
    #x9FC10D0F989993E0 #xDE68A2355B93CAE6 #xA44CFE79AE538BBE #x9D1D84FCCE371425
    #x51D2B1AB2DDFB636 #x2FD7E4B9E72CD38C #x65CA5B96B7552210 #xDD69A0D8AB3B546D
    #x604D51B25FBF70E2 #x73AA8A564FB7AC9E #x1A8C1E992B941148 #xAAC40A2703D9BEA0
    #x764DBEAE7FA4F3A6 #x1E99B96E70A9BE8B #x2C5E9DEB57EF4743 #x3A938FEE32D29981
    #x26E6DB8FFDF5ADFE #x469356C504EC9F9D #xC8763C5B08D1908C #x3F6C6AF859D80055
    #x7F7CC39420A3A545 #x9BFB227EBDF4C5CE #x89039D79D6FC5C5C #x8FE88B57305E2AB6
    #xA09E8C8C35AB96DE #xFA7E393983325753 #xD6B6D0ECC617C699 #xDFEA21EA9E7557E3
    #xB67C1FA481680AF8 #xCA1E3785A9E724E5 #x1CFC8BED0D681639 #xD18D8549D140CAEA
    #x4ED0FE7E9DC91335 #xE4DBF0634473F5D2 #x1761F93A44D5AEFE #x53898E4C3910DA55
    #x734DE8181F6EC39A #x2680B122BAA28D97 #x298AF231C85BAFAB #x7983EED3740847D5
    #x66C1A2A1A60CD889 #x9E17E49642A3E4C1 #xEDB454E7BADC0805 #x50B704CAB602C329
    #x4CC317FB9CDDD023 #x66B4835D9EAFEA22 #x219B97E26FFC81BD #x261E4E4C0A333A9D
    #x1FE2CCA76517DB90 #xD7504DFA8816EDBB #xB9571FA04DC089C8 #x1DDC0325259B27DE
    #xCF3F4688801EB9AA #xF4F5D05C10CAB243 #x38B6525C21A42B0E #x36F60E2BA4FA6800
    #xEB3593803173E0CE #x9C4CD6257C5A3603 #xAF0C317D32ADAA8A #x258E5A80C7204C4B
    #x8B889D624D44885D #xF4D14597E660F855 #xD4347F66EC8941C3 #xE699ED85B0DFB40D
    #x2472F6207C2D0484 #xC2A1E7B5B459AEB5 #xAB4F6451CC1D45EC #x63767572AE3D6174
    #xA59E0BD101731A28 #x116D0016CB948F09 #x2CF9C8CA052F6E9F #x0B090A7560A968E3
    #xABEEDDB2DDE06FF1 #x58EFC10B06A2068D #xC6E57A78FBD986E0 #x2EAB8CA63CE802D7
    #x14A195640116F336 #x7C0828DD624EC390 #xD74BBE77E6116AC7 #x804456AF10F5FB53
    #xEBE9EA2ADF4321C7 #x03219A39EE587A30 #x49787FEF17AF9924 #xA1E9300CD8520548
    #x5B45E522E4B1B4EF #xB49C3B3995091A36 #xD4490AD526F14431 #x12A8F216AF9418C2
    #x001F837CC7350524 #x1877B51E57A764D5 #xA2853B80F17F58EE #x993E1DE72D36D310
    #xB3598080CE64A656 #x252F59CF0D9F04BB #xD23C8E176D113600 #x1BDA0492E7E4586E
    #x21E0BD5026C619BF #x3B097ADAF088F94E #x8D14DEDB30BE846E #xF95CFFA23AF5F6F4
    #x3871700761B3F743 #xCA672B91E9E4FA16 #x64C8E531BFF53B55 #x241260ED4AD1E87D
    #x106C09B972D2E822 #x7FBA195410E5CA30 #x7884D9BC6CB569D8 #x0647DFEDCD894A29
    #x63573FF03E224774 #x4FC8E9560F91B123 #x1DB956E450275779 #xB8D91274B9E9D4FB
    #xA2EBEE47E2FBFCE1 #xD9F1F30CCD97FB09 #xEFED53D75FD64E6B #x2E6D02C36017F67F
    #xA9AA4D20DB084E9B #xB64BE8D8B25396C1 #x70CB6AF7C2D5BCF0 #x98F076A4F7A2322E
    #xBF84470805E69B5F #x94C3251F06F90CF3 #x3E003E616A6591E9 #xB925A6CD0421AFF3
    #x61BDD1307C66E300 #xBF8D5108E27E0D48 #x240AB57A8B888B20 #xFC87614BAF287E07
    #xEF02CDD06FFDB432 #xA1082C0466DF6C0A #x8215E577001332C8 #xD39BB9C3A48DB6CF
    #x2738259634305C14 #x61CF4F94C97DF93D #x1B6BACA2AE4E125B #x758F450C88572E0B
    #x959F587D507A8359 #xB063E962E045F54D #x60E8ED72C0DFF5D1 #x7B64978555326F9F
    #xFD080D236DA814BA #x8C90FD9B083F4558 #x106F72FE81E2C590 #x7976033A39F7D952
    #xA4EC0132764CA04B #x733EA705FAE4FA77 #xB4D8F77BC3E56167 #x9E21F4F903B33FD9
    #x9D765E419FB69F6D #xD30C088BA61EA5EF #x5D94337FBFAF7F5B #x1A4E4822EB4D7A59
    #x6FFE73E81B637FB3 #xDDF957BC36D8B9CA #x64D0E29EEA8838B3 #x08DD9BDFD96B9F63
    #x087E79E5A57D1D13 #xE328E230E3E2B3FB #x1C2559E30F0946BE #x720BF5F26F4D2EAA
    #xB0774D261CC609DB #x443F64EC5A371195 #x4112CF68649A260E #xD813F2FAB7F5C5CA
    #x660D3257380841EE #x59AC2C7873F910A3 #xE846963877671A17 #x93B633ABFA3469F8
    #xC0C0F5A60EF4CDCF #xCAF21ECD4377B28C #x57277707199B8175 #x506C11B9D90E8B1D
    #xD83CC2687A19255F #x4A29C6465A314CD1 #xED2DF21216235097 #xB5635C95FF7296E2
    #x22AF003AB672E811 #x52E762596BF68235 #x9AEBA33AC6ECC6B0 #x944F6DE09134DFB6
    #x6C47BEC883A7DE39 #x6AD047C430A12104 #xA5B1CFDBA0AB4067 #x7C45D833AFF07862
    #x5092EF950A16DA0B #x9338E69C052B8E7B #x455A4B4CFE30E3F5 #x6B02E63195AD0CF8
    #x6B17B224BAD6BF27 #xD1E0CCD25BB9C169 #xDE0C89A556B9AE70 #x50065E535A213CF6
    #x9C1169FA2777B874 #x78EDEFD694AF1EED #x6DC93D9526A50E68 #xEE97F453F06791ED
    #x32AB0EDB696703D3 #x3A6853C7E70757A7 #x31865CED6120F37D #x67FEF95D92607890
    #x1F2B1D1F15F6DC9C #xB69E38A8965C6B65 #xAA9119FF184CCCF4 #xF43C732873F24C13
    #xFB4A3D794A9A80D2 #x3550C2321FD6109C #x371F77E76BB8417E #x6BFA9AAE5EC05779
    #xCD04F3FF001A4778 #xE3273522064480CA #x9F91508BFFCFC14A #x049A7F41061A9E60
    #xFCB6BE43A9F2FE9B #x08DE8A1C7797DA9B #x8F9887E6078735A1 #xB5B4071DBFC73A66
    #x230E343DFBA08D33 #x43ED7F5A0FAE657D #x3A88A0FBBCB05C63 #x21874B8B4D2DBC4F
    #x1BDEA12E35F6A8C9 #x53C065C6C8E63528 #xE34A1D250E7A8D6B #xD6B04D3B7651DD7E
    #x5E90277E7CB39E2D #x2C046F22062DC67D #xB10BB459132D0A26 #x3FA9DDFB67E2F199
    #x0E09B88E1914F7AF #x10E8B35AF3EEAB37 #x9EEDECA8E272B933 #xD4C718BC4AE8AE5F
    #x81536D601170FC20 #x91B534F885818A06 #xEC8177F83F900978 #x190E714FADA5156E
    #xB592BF39B0364963 #x89C350C893AE7DC1 #xAC042E70F8B383F2 #xB49B52E587A1EE60
    #xFB152FE3FF26DA89 #x3E666E6F69AE2C15 #x3B544EBE544C19F9 #xE805A1E290CF2456
    #x24B33C9D7ED25117 #xE74733427B72F0C1 #x0A804D18B7097475 #x57E3306D881EDB4F
    #x4AE7D6A36EB5DBCB #x2D8D5432157064C8 #xD1E649DE1E7F268B #x8A328A1CEDFE552C
    #x07A3AEC79624C7DA #x84547DDC3E203C94 #x990A98FD5071D263 #x1A4FF12616EEFC89
    #xF6F7FD1431714200 #x30C05B1BA332F41C #x8D2636B81555A786 #x46C9FEB55D120902
    #xCCEC0A73B49C9921 #x4E9D2827355FC492 #x19EBB029435DCB0F #x4659D2B743848A2C
    #x963EF2C96B33BE31 #x74F85198B05A2E7D #x5A0F544DD2B1FB18 #x03727073C2E134B1
    #xC7F6AA2DE59AEA61 #x352787BAA0D7C22F #x9853EAB63B5E0B35 #xABBDCDD7ED5C0860
    #xCF05DAF5AC8D77B0 #x49CAD48CEBF4A71E #x7A4C10EC2158C4A6 #xD9E92AA246BF719E
    #x13AE978D09FE5557 #x730499AF921549FF #x4E4B705B92903BA4 #xFF577222C14F0A3A
    #x55B6344CF97AAFAE #xB862225B055B6960 #xCAC09AFBDDD2CDB4 #xDAF8E9829FE96B5F
    #xB5FDFC5D3132C498 #x310CB380DB6F7503 #xE87FBB46217A360E #x2102AE466EBB1148
    #xF8549E1A3AA5E00D #x07A69AFDCC42261A #xC4C118BFE78FEAAE #xF9F4892ED96BD438
    #x1AF3DBE25D8F45DA #xF5B4B0B0D2DEEEB4 #x962ACEEFA82E1C84 #x046E3ECAAF453CE9
    #xF05D129681949A4C #x964781CE734B3C84 #x9C2ED44081CE5FBD #x522E23F3925E319E
    #x177E00F9FC32F791 #x2BC60A63A6F3B3F2 #x222BBFAE61725606 #x486289DDCC3D6780
    #x7DC7785B8EFDFC80 #x8AF38731C02BA980 #x1FAB64EA29A2DDF7 #xE4D9429322CD065A
    #x9DA058C67844F20C #x24C0E332B70019B0 #x233003B5A6CFE6AD #xD586BD01C5C217F6
    #x5E5637885F29BC2B #x7EBA726D8C94094B #x0A56A5F0BFE39272 #xD79476A84EE20D06
    #x9E4C1269BAA4BF37 #x17EFEE45B0DEE640 #x1D95B0A5FCF90BC6 #x93CBE0B699C2585D
    #x65FA4F227A2B6D79 #xD5F9E858292504D5 #xC2B5A03F71471A6F #x59300222B4561E00
    #xCE2F8642CA0712DC #x7CA9723FBB2E8988 #x2785338347F2BA08 #xC61BB3A141E50E8C
    #x150F361DAB9DEC26 #x9F6A419D382595F4 #x64A53DC924FE7AC9 #x142DE49FFF7A7C3D
    #x0C335248857FA9E7 #x0A9C32D5EAE45305 #xE6C42178C4BBB92E #x71F1CE2490D20B07
    #xF1BCC3D275AFE51A #xE728E8C83C334074 #x96FBF83A12884624 #x81A1549FD6573DA5
    #x5FA7867CAF35E149 #x56986E2EF3ED091B #x917F1DD5F8886C61 #xD20D8C88C8FFE65F
    #x31D71DCE64B2C310 #xF165B587DF898190 #xA57E6339DD2CF3A0 #x1EF6E6DBB1961EC9
    #x70CC73D90BC26E24 #xE21A6B35DF0C3AD7 #x003A93D8B2806962 #x1C99DED33CB890A1
    #xCF3145DE0ADD4289 #xD0E4427A5514FB72 #x77C621CC9FB3A483 #x67A34DAC4356550B
    #xF8D626AAAF278509))


(defmacro enumerate (start &body elements)
  (append
   '(progn)
   (loop for i from start
         for e in elements
         collect `(define-constant ,e ,i))))

(enumerate 0
  z-black-pawn
  z-white-pawn
  z-black-knight
  z-white-knight
  z-black-bishop
  z-white-bishop
  z-black-rook
  z-white-rook
  z-black-queen
  z-white-queen
  z-black-king
  z-white-king
  )

(defun piece->zobrist-hash-enum (p)
  (declare (type piece p))
  (if (whitep p)
      (case (piece-type p)
        (pawn   z-white-pawn)
        (rook   z-white-rook)
        (bishop z-white-bishop)
        (knight z-white-knight)
        (queen  z-white-queen)
        (king   z-white-king))
      (case (piece-type p)
        (pawn   z-black-pawn)
        (rook   z-black-rook)
        (bishop z-black-bishop)
        (knight z-black-knight)
        (queen  z-black-queen)
        (king   z-black-king))))

(defun z-maybe-en-passant (game)
  (if-let ((ts (game-en-passant-target-square game)))
    (if (or (piece-at-point game (- (car ts) 1) (cadr ts))
            (piece-at-point game (+ (car ts) 1) (cadr ts)))
        (aref *random-table* (+ (car ts) 772))
        0)
    0))

(defun hash-zobrist (game)
  (declare (type game game)
           (values (unsigned-byte 64)))
  (let ((h (the (unsigned-byte 64) 0)))
    (setf h (logxor
             (ifz (game-white-can-castle-kingside-p game)  (aref *random-table* (+ 768 0)))
             (ifz (game-white-can-castle-queenside-p game) (aref *random-table* (+ 768 1)))
             (ifz (game-black-can-castle-kingside-p game)  (aref *random-table* (+ 768 2)))
             (ifz (game-black-can-castle-queenside-p game) (aref *random-table* (+ 768 3)))
             (z-maybe-en-passant game)
             (ifz (game-turn-white-p game) (aref *random-table* 780))))
    (loop for p in (game-pieces game) do
      (setf h (logxor h (aref *random-table* (+ (* 64 (piece->zobrist-hash-enum p)) (* 8 (- 7 (point-y (piece-point p)))) (point-x (piece-point p)))))))
    h))

;;; pgn reader

(defstruct (pgn (:conc-name pgn-))
  (event            "" :type string)
  (site             "" :type string)
  (date             "" :type string)
  (round            "" :type string)
  (white            "" :type string)
  (black            "" :type string)
  (result           "" :type string)
  (additional-tags nil :type list)
  (moves           nil :type list))

(defmacro cnump (c)
  (let ((s (gensym)))
    `(let ((,s ,c))
       (and (>= (char-int ,s) (char-int #\0))
            (<= (char-int ,s) (char-int #\9))))))

(defmacro cchrp (c)
  (let ((s (gensym)))
    `(let ((,s ,c))
       (and (>= (char-int ,s) (char-int #\a))
            (<= (char-int ,s) (char-int #\h))))))

;; unsafe
(defmacro s-begins-with (s with)
  `(if (> (length ,with) (length ,s))
       nil
       (string= ,s ,with :end1 (length ,with))))

;; str is a string containing the algebraic move
;; TODO: pawn upgrading to type
(defun algebraic->lst (g str)
  (declare (type game g)
           (type string str)
           (values list))          ; ((x1 y1) (x2 y2) additional-data)
  (if (eq (aref str 0) #\O)
      (if (s-begins-with str "O-O-O")
          (if (game-turn-white-p g)
              '((4 7) (2 7))
              '((4 0) (2 0)))
          (if (game-turn-white-p g)
              '((4 7) (6 7))
              '((4 0) (6 0))))
      (let-values ((type str
                         (case (aref str 0)
                           (#\N (values 'knight (subseq str 1)))
                           (#\B (values 'bishop (subseq str 1)))
                           (#\R (values 'rook   (subseq str 1)))
                           (#\Q (values 'queen  (subseq str 1)))
                           (#\K (values 'king   (subseq str 1)))
                           (#\P (values 'pawn   (subseq str 1)))
                           (t   (values 'pawn   str)))))
        (let ((str (delete #\x str))) ; skip x as we don't care
          (cond
            ((and (>= (length str) 4) (cchrp (aref str 0)) (cnump (aref str 1)) (cchrp (aref str 2)) (cnump (aref str 3))) ; exact disambiguation
             (list
              (pos->lst (coerce (subseq str 0 2) 'string))
              (pos->lst (coerce (subseq str 2 4) 'string))))
            ((and (>= (length str) 3) (cchrp (aref str 0)) (cchrp (aref str 1)) (cnump (aref str 2))) ; file disambiguation
             (let* ((x (- (char-int (aref str 0)) (char-int #\a)))
                    (target (pos->lst (coerce (subseq str 1 3) 'string)))
                    (l (remove-if-not
                        #'(lambda (p)
                            (and
                             (eq (piece-type p) type)
                             (= (point-x (piece-point p)) x)
                             (move-possible-p p (car target) (cadr target) g)))
                        (game-pieces g))))
               (if (= (length l) 1)
                   `((,(point-x (piece-point (car l))) ,(point-y (piece-point (car l)))) ,target)
                   (error "Couldn't find piece that has to be moved to ~a, possible candidates: ~a." (subseq str 1 3) l))))
            ((and (>= (length str) 3) (cnump (aref str 0)) (cchrp (aref str 1)) (cnump (aref str 2))) ; rank disambiguation
             (let* ((y (- 8 (- (char-int (aref str 0)) (char-int #\0))))
                    (target (pos->lst (coerce (subseq str 1 3) 'string)))
                    (l (remove-if-not
                        #'(lambda (p)
                            (and
                             (eq (piece-type p) type)
                             (= (point-y (piece-point p)) y)
                             (move-possible-p p (car target) (cadr target) g)))
                        (game-pieces g))))
               (if (= (length l) 1)
                   `((,(point-x (piece-point (car l))) ,(point-y (piece-point (car l)))) ,target)
                   (error "Couldn't find piece that has to be moved to ~a, possible candidates: ~a." (subseq str 1 3) l))))
            ((and (cchrp (aref str 0)) (cnump (aref str 1))) ; no disambiguation
             (let* ((target (pos->lst (coerce (subseq str 0 2) 'string)))
                    (l (remove-if-not
                        #'(lambda (p)
                            (and
                             (eq (piece-type p) type)
                             (move-possible-p p (car target) (cadr target) g)))
                        (game-pieces g))))
               (if (= (length l) 1)
                   `((,(point-x (piece-point (car l))) ,(point-y (piece-point (car l)))) ,target)
                   (error "Couldn't find piece that has to be moved to ~a, possible candidates: ~a." (subseq str 0 2) l))))
            (t
             (error "Malformed pgn data in ~a." str)))))))


;; Read pgn data from list of lines upto end of game, returns (values (pgn rest))
;; warning: hacky
(defun read-pgn (lst)
  (declare (type list lst)
           (values pgn list))
  (let ((p (make-pgn))
        (g (fen->game +initial-fen+)))
    (initialize-game g 'white nil)
    (loop while (not (equal (car lst) "")) ; get STR
          do
             (let* ((s (car lst))
                    (l (read-from-string (format nil "(~a)" (subseq s 1 (- (length s) 1)))))) ; <- LMAO
               (case (car l)
                 (event  (setf (pgn-event p) (cadr l)))
                 (site   (setf (pgn-site p) (cadr l)))
                 (date   (setf (pgn-date p) (cadr l)))
                 (round  (setf (pgn-round p) (cadr l)))
                 (white  (setf (pgn-white p) (cadr l)))
                 (black  (setf (pgn-black p) (cadr l)))
                 (result (setf (pgn-result p) (cadr l)))
                 (t
                  (push (cons (car l) (cadr l)) (pgn-additional-tags p))))
               (setf lst (cdr lst))))
    (setf lst (cdr lst))
    (let ((moves nil))
      (loop while (and (not (equal (car lst) "")) lst) ; get movetext
            do
               (setf moves (append
                            moves
                            (remove-if
                             #'(lambda (s)
                                 (or
                                  (equal s "")
                                  (equal s "1/2-1/2")
                                  (equal s "1-0")
                                  (equal s "0-1")
                                  (equal s "*")))
                             (cl-ppcre:split
                              "(?:(?:\\s+)|(?:{.*?})|(?:\\d+\\.+))+"
                              (car lst)))))
               (setf lst (cdr lst)))
      (loop for m in moves do
        (let* ((ml (algebraic->lst g m))
               (from (car ml))
               (to (cadr ml)))
          (game-do-move g (piece-at-point g (car from) (cadr from)) (car to) (cadr to) :no-display-check-mates t :no-history t :no-update-timers t)
          (setf (pgn-moves p) (append (pgn-moves p) (list ml))))))
    (values p (cdr lst))))

;; this can be done in read-pgn to save time
;; but this is done at compile-time so idk if i should bother
;; do %s/[^ -~\n\r]//g if uiop:read-file-lines whines about ascii
(defun gm-book->ht (filename &optional n)
  (let ((data (uiop:read-file-lines filename))
        (ht (make-hash-table)))
    (loop while data do
      (handler-case
          (let-values ((pgn rest (read-pgn data))
                       (g (fen->game +initial-fen+))
                       (moves (pgn-moves pgn)))
            (initialize-game g 'white nil)
            (loop for i from 0
                  while (and (< i 20) moves)
                  do
                     (let* ((m (car moves))
                            (from (car m))
                            (to (cadr m))
                            (h (hash-zobrist g))
                            (old (gethash h ht)))
                       (when (not (hasp m old))
                         (setf (gethash h ht) (append old (list m))))
                       (game-do-move g (piece-at-point g (car from) (cadr from)) (car to) (cadr to))
                       (setf moves (cdr moves))))
            (when n
              (format t "left: ~a~%" n)
              (decf n))
            (setf data rest))
        (t (e) ; something i didn't handle
               ; but there are so many games i can just skip it lmao
          (warn "caught: ~a" e)
          (dotimes (_ 2)
            (loop while (and (not (equal (car data) "")) data)
                  do
                     (setf data (cdr data)))
            (setf data (cdr data)))
          (format t "after error beginning of data is ~a~%" (car data)))))
    ht))

(defun load-gm-ht (gm &key flush)
  (declare (type string gm))
  (let ((dat (format nil "res/dat/~a.dat" gm))
        (pgn (format nil "res/pgn/~a.pgn" gm)))

    (if (and (uiop/filesystem:file-exists-p dat) (not flush))
        (cl-store:restore dat)
        (let ((ht (gm-book->ht pgn)))
          (cl-store:store ht dat)
          ht))))

(defparameter *kasparov-book*   (load-gm-ht "Kasparov"))
(defparameter *alekhine-book*   (load-gm-ht "Alekhine"))
(defparameter *anand-book*      (load-gm-ht "Anand"))
(defparameter *botvinnik-book*  (load-gm-ht "Botvinnik"))
(defparameter *capablanca-book* (load-gm-ht "Capablanca"))
(defparameter *carlsen-book*    (load-gm-ht "Carlsen"))
(defparameter *caruana-book*    (load-gm-ht "Caruana"))
(defparameter *fischer-book*    (load-gm-ht "Fischer"))
(defparameter *morphy-book*     (load-gm-ht "Morphy"))
(defparameter *nakamura-book*   (load-gm-ht "Nakamura"))
(defparameter *polgarj-book*    (load-gm-ht "PolgarJ"))
(defparameter *tal-book*        (load-gm-ht "Tal"))

(defparameter %player-vs-bot        (make-player-vs-bot *book*))
(defparameter %player-vs-kasparov   (make-player-vs-bot *kasparov-book*))
(defparameter %player-vs-alekhine   (make-player-vs-bot *alekhine-book*))
(defparameter %player-vs-anand      (make-player-vs-bot *anand-book*))
(defparameter %player-vs-botvinnik  (make-player-vs-bot *botvinnik-book*))
(defparameter %player-vs-capablanca (make-player-vs-bot *capablanca-book*))
(defparameter %player-vs-carlsen    (make-player-vs-bot *carlsen-book*))
(defparameter %player-vs-caruana    (make-player-vs-bot *caruana-book*))
(defparameter %player-vs-fischer    (make-player-vs-bot *fischer-book*))
(defparameter %player-vs-morphy     (make-player-vs-bot *morphy-book*))
(defparameter %player-vs-nakamura   (make-player-vs-bot *nakamura-book*))
(defparameter %player-vs-polgarj    (make-player-vs-bot *polgarj-book*))
(defparameter %player-vs-tal        (make-player-vs-bot *tal-book*))
