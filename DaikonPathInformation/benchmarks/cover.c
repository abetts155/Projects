/*
 * Cover taken from MDH suite and modified by Adam Betts to consume a
 * test vector supplied on the command line. Note that the three original functions
 * 'swi10', 'swi50', 'swi120' have all been merged into the single 'cover' function
 * because they each have a loop with a fixed number of iterations, and the paths
 * followed through each do not depend on input or a particular calling order
 *
 * For this program, a one-element test vector is expected.
 */
 
int 
swi120 (int c)
{
 #ifdef CBMC
//==========> swi120 : header 126
int __count_3_4 = 0;
int __count_3_5 = 0;
int __count_3_6 = 0;
int __count_3_7 = 0;
int __count_3_9 = 0;
int __count_3_12 = 0;
int __count_3_13 = 0;
int __count_3_14 = 0;
int __count_3_17 = 0;
int __count_3_20 = 0;
int __count_3_21 = 0;
int __count_3_22 = 0;
int __count_3_25 = 0;
int __count_3_28 = 0;
int __count_3_29 = 0;
int __count_3_30 = 0;
int __count_3_33 = 0;
int __count_3_36 = 0;
int __count_3_37 = 0;
int __count_3_38 = 0;
int __count_3_41 = 0;
int __count_3_44 = 0;
int __count_3_45 = 0;
int __count_3_46 = 0;
int __count_3_49 = 0;
int __count_3_52 = 0;
int __count_3_53 = 0;
int __count_3_54 = 0;
int __count_3_57 = 0;
int __count_3_60 = 0;
int __count_3_61 = 0;
int __count_3_62 = 0;
int __count_3_65 = 0;
int __count_3_68 = 0;
int __count_3_69 = 0;
int __count_3_70 = 0;
int __count_3_73 = 0;
int __count_3_76 = 0;
int __count_3_77 = 0;
int __count_3_78 = 0;
int __count_3_81 = 0;
int __count_3_84 = 0;
int __count_3_85 = 0;
int __count_3_86 = 0;
int __count_3_89 = 0;
int __count_3_92 = 0;
int __count_3_93 = 0;
int __count_3_94 = 0;
int __count_3_97 = 0;
int __count_3_100 = 0;
int __count_3_101 = 0;
int __count_3_102 = 0;
int __count_3_105 = 0;
int __count_3_108 = 0;
int __count_3_109 = 0;
int __count_3_110 = 0;
int __count_3_113 = 0;
int __count_3_116 = 0;
int __count_3_117 = 0;
int __count_3_118 = 0;
int __count_3_121 = 0;
int __count_8_125 = 0;
int __count_10_125 = 0;
int __count_11_125 = 0;
int __count_15_125 = 0;
int __count_16_125 = 0;
int __count_18_125 = 0;
int __count_19_125 = 0;
int __count_23_125 = 0;
int __count_24_125 = 0;
int __count_26_125 = 0;
int __count_27_125 = 0;
int __count_31_125 = 0;
int __count_32_125 = 0;
int __count_34_125 = 0;
int __count_35_125 = 0;
int __count_39_125 = 0;
int __count_40_125 = 0;
int __count_42_125 = 0;
int __count_43_125 = 0;
int __count_47_125 = 0;
int __count_48_125 = 0;
int __count_50_125 = 0;
int __count_51_125 = 0;
int __count_55_125 = 0;
int __count_56_125 = 0;
int __count_58_125 = 0;
int __count_59_125 = 0;
int __count_63_125 = 0;
int __count_64_125 = 0;
int __count_66_125 = 0;
int __count_67_125 = 0;
int __count_71_125 = 0;
int __count_72_125 = 0;
int __count_74_125 = 0;
int __count_75_125 = 0;
int __count_79_125 = 0;
int __count_80_125 = 0;
int __count_82_125 = 0;
int __count_83_125 = 0;
int __count_87_125 = 0;
int __count_88_125 = 0;
int __count_90_125 = 0;
int __count_91_125 = 0;
int __count_95_125 = 0;
int __count_96_125 = 0;
int __count_98_125 = 0;
int __count_99_125 = 0;
int __count_103_125 = 0;
int __count_104_125 = 0;
int __count_106_125 = 0;
int __count_107_125 = 0;
int __count_111_125 = 0;
int __count_112_125 = 0;
int __count_114_125 = 0;
int __count_115_125 = 0;
int __count_119_125 = 0;
int __count_120_125 = 0;
int __count_122_125 = 0;
int __count_123_125 = 0;
int __count_124_125 = 0;
int __count_126_2 = 0; //Loop counter
//==========> swi120 : header 1
int __count_127 = 0;
int __count_126_127 = 0;
#endif

  int i;
  
  for (i = 0; i < 120; i++)
  {
    #ifdef CBMC
    __count_126_2++;
    #endif
    switch (i)
    {
      case 0:
      #ifdef CBMC
      __count_3_4++;
      #endif
        c++;
        break;
      case 1:
      #ifdef CBMC
      __count_3_5++;
      #endif
        c++;
        break;
      case 2:
      #ifdef CBMC
      __count_3_6++;
      #endif
        c++;
        break;
      case 3:
      #ifdef CBMC
      __count_3_7++;
      #endif
        c++;
        break;
      case 4:
      #ifdef CBMC
      __count_8_125++;
      #endif
        c++;
        break;
      case 5:
      #ifdef CBMC
      __count_3_9++;
      #endif
        c++;
        break;
      case 6:
      #ifdef CBMC
       __count_10_125++;
      #endif
        c++;
        break;
      case 7:
      #ifdef CBMC
       __count_11_125++;
      #endif
        c++;
        break;
      case 8:
      #ifdef CBMC
      __count_3_12++;
      #endif
        c++;
        break;
      case 9:
      #ifdef CBMC
      __count_3_13++;
      #endif
        c++;
        break;
      case 10:
      #ifdef CBMC
      __count_3_14++;
      #endif
        c++;
        break;
      case 11:
      #ifdef CBMC
       __count_15_125++;
      #endif
        c++;
        break;
      case 12:
      #ifdef CBMC
       __count_16_125++;
      #endif
        c++;
        break;
      case 13:
      #ifdef CBMC
      __count_3_17++;
      #endif
        c++;
        break;
      case 14:
      #ifdef CBMC
       __count_18_125++;
      #endif
        c++;
        break;
      case 15:
      #ifdef CBMC
       __count_19_125++;
      #endif
        c++;
        break;
      case 16:
      #ifdef CBMC
       __count_3_20++;
      #endif
        c++;
        break;
      case 17:
      #ifdef CBMC
      __count_3_21++;
      #endif
        c++;
        break;
      case 18:
      #ifdef CBMC
      __count_3_22++;
      #endif
        c++;
        break;
      case 19:
      #ifdef CBMC
       __count_23_125++;
      #endif
        c++;
        break;
      case 20:
      #ifdef CBMC
       __count_24_125++;
      #endif
        c++;
        break;
      case 21:
      #ifdef CBMC
       __count_3_25++;
      #endif
        c++;
        break;
      case 22:
      #ifdef CBMC
       __count_26_125++;
      #endif
        c++;
        break;
      case 23:
      #ifdef CBMC
       __count_27_125++;
      #endif
        c++;
        break;
      case 24:
      #ifdef CBMC
      __count_3_28++;
      #endif
        c++;
        break;
      case 25:
      #ifdef CBMC
      __count_3_29++;
      #endif
        c++;
        break;
      case 26:
      #ifdef CBMC
      __count_3_30++;
      #endif
        c++;
        break;
      case 27:
      #ifdef CBMC
      __count_31_125++;
      #endif
        c++;
        break;
      case 28:
      #ifdef CBMC
      __count_32_125++;
      #endif
        c++;
        break;
      case 29:
      #ifdef CBMC
      __count_3_33++;
      #endif
        c++;
        break;
      case 30:
      #ifdef CBMC
      __count_34_125++;
      #endif
        c++;
        break;
      case 31:
      #ifdef CBMC
      __count_35_125++;
      #endif
        c++;
        break;
      case 32:
      #ifdef CBMC
      __count_3_36++;
      #endif
        c++;
        break;
      case 33:
      #ifdef CBMC
      __count_3_37++;
      #endif
        c++;
        break;
      case 34:
      #ifdef CBMC
      __count_3_38++;
      #endif
        c++;
        break;
      case 35:
      #ifdef CBMC
      __count_39_125++;
      #endif
        c++;
        break;
      case 36:
      #ifdef CBMC
      __count_40_125++;
      #endif
        c++;
        break;
      case 37:
      #ifdef CBMC
      __count_3_41++;
      #endif
        c++;
        break;
      case 38:
      #ifdef CBMC
      __count_42_125++;
      #endif
        c++;
        break;
      case 39:
      #ifdef CBMC
      __count_43_125++;
      #endif
        c++;
        break;
      case 40:
      #ifdef CBMC
      __count_3_44++;
      #endif
        c++;
        break;
      case 41:
      #ifdef CBMC
      __count_3_45++;
      #endif
        c++;
        break;
      case 42:
      #ifdef CBMC
      __count_3_46++;
      #endif
        c++;
        break;
      case 43:
      #ifdef CBMC
      __count_47_125++;
      #endif
        c++;
        break;
      case 44:
      #ifdef CBMC
      __count_48_125++;
      #endif
        c++;
        break;
      case 45:
      #ifdef CBMC
      __count_3_49++;
      #endif
        c++;
        break;
      case 46:
      #ifdef CBMC
      __count_50_125++;
      #endif
        c++;
        break;
      case 47:
      #ifdef CBMC
      __count_51_125++;
      #endif
        c++;
        break;
      case 48:
      #ifdef CBMC
      __count_3_52++;
      #endif
        c++;
        break;
      case 49:
      #ifdef CBMC
      __count_3_53++;
      #endif
        c++;
        break;
      case 50:
      #ifdef CBMC
      __count_3_54++;
      #endif
        c++;
        break;
      case 51:
      #ifdef CBMC
      __count_55_125++;
      #endif
        c++;
        break;
      case 52:
      #ifdef CBMC
      __count_56_125++;
      #endif
        c++;
        break;
      case 53:
      #ifdef CBMC
      __count_3_57++;
      #endif
        c++;
        break;
      case 54:
      #ifdef CBMC
      __count_58_125++;
      #endif
        c++;
        break;
      case 55:
      #ifdef CBMC
      __count_59_125++;
      #endif
        c++;
        break;
      case 56:
      #ifdef CBMC
      __count_3_60++;
      #endif
        c++;
        break;
      case 57:
      #ifdef CBMC
      __count_3_61++;
      #endif
        c++;
        break;
      case 58:
      #ifdef CBMC
      __count_3_62++;
      #endif
        c++;
        break;
      case 59:
      #ifdef CBMC
      __count_63_125++;
      #endif
        c++;
        break;
      case 60:
      #ifdef CBMC
      __count_64_125++;
      #endif
        c++;
        break;
      case 61:
      #ifdef CBMC
      __count_3_65++;
      #endif
        c++;
        break;
      case 62:
      #ifdef CBMC
      __count_66_125++;
      #endif
        c++;
        break;
      case 63:
      #ifdef CBMC
      __count_67_125++;
      #endif
        c++;
        break;
      case 64:
      #ifdef CBMC
      __count_3_68++;
      #endif
        c++;
        break;
      case 65:
      #ifdef CBMC
      __count_3_69++;
      #endif
        c++;
        break;
      case 66:
      #ifdef CBMC
      __count_3_70++;
      #endif
        c++;
        break;
      case 67:
      #ifdef CBMC
      __count_71_125++;
      #endif
        c++;
        break;
      case 68:
      #ifdef CBMC
      __count_72_125++;
      #endif
        c++;
        break;
      case 69:
      #ifdef CBMC
      __count_3_73++;
      #endif
        c++;
        break;
      case 70:
      #ifdef CBMC
      __count_74_125++;
      #endif
        c++;
        break;
      case 71:
      #ifdef CBMC
      __count_75_125++;
      #endif
        c++;
        break;
      case 72:
      #ifdef CBMC
      __count_3_76++;
      #endif
        c++;
        break;
      case 73:
      #ifdef CBMC
      __count_3_77++;
      #endif
        c++;
        break;
      case 74:
      #ifdef CBMC
      __count_3_78++;
      #endif
        c++;
        break;
      case 75:
      #ifdef CBMC
      __count_79_125++;
      #endif
        c++;
        break;
      case 76:
      #ifdef CBMC
      __count_80_125++;
      #endif
        c++;
        break;
      case 77:
      #ifdef CBMC
       __count_3_81++;
      #endif
        c++;
        break;
      case 78:
      #ifdef CBMC
      __count_82_125++;
      #endif
        c++;
        break;
      case 79:
      #ifdef CBMC
      __count_83_125++;
      #endif
        c++;
        break;
      case 80:
      #ifdef CBMC
       __count_3_84++;
      #endif
        c++;
        break;
      case 81:
      #ifdef CBMC
       __count_3_85++;
      #endif
        c++;
        break;
      case 82:
      #ifdef CBMC
       __count_3_86++;
      #endif
        c++;
        break;
      case 83:
      #ifdef CBMC
      __count_87_125++;
      #endif
        c++;
        break;
      case 84:
      #ifdef CBMC
      __count_88_125++;
      #endif
        c++;
        break;
      case 85:
      #ifdef CBMC
       __count_3_89++;
      #endif
        c++;
        break;
      case 86:
      #ifdef CBMC
      __count_90_125++;
      #endif
        c++;
        break;
      case 87:
      #ifdef CBMC
      __count_91_125++;
      #endif
        c++;
        break;
      case 88:
      #ifdef CBMC
      __count_3_92++;
      #endif
        c++;
        break;
      case 89:
      #ifdef CBMC
      __count_3_93++;
      #endif
        c++;
        break;
      case 90:
      #ifdef CBMC
      __count_3_94++;
      #endif
        c++;
        break;
      case 91:
      #ifdef CBMC
      __count_95_125++;
      #endif
        c++;
        break;
      case 92:
      #ifdef CBMC
      __count_96_125++;
      #endif
        c++;
        break;
      case 93:
      #ifdef CBMC
      __count_3_97++;
      #endif
        c++;
        break;
      case 94:
      #ifdef CBMC
      __count_98_125++;
      #endif
        c++;
        break;
      case 95:
      #ifdef CBMC
      __count_99_125++;
      #endif
        c++;
        break;
      case 96:
      #ifdef CBMC
      __count_3_100++;
      #endif
        c++;
        break;
      case 97:
      #ifdef CBMC
      __count_3_101++;
      #endif
        c++;
        break;
      case 98:
      #ifdef CBMC
      __count_3_102++;
      #endif
        c++;
        break;
      case 99:
      #ifdef CBMC
      __count_103_125++; 
      #endif
        c++;
        break;
      case 100:
      #ifdef CBMC
      __count_104_125++;
      #endif
        c++;
        break;
      case 101:
      #ifdef CBMC
      __count_3_105++;
      #endif
        c++;
        break;
      case 102:
      #ifdef CBMC
      __count_106_125++;
      #endif
        c++;
        break;
      case 103:
      #ifdef CBMC
      __count_107_125++;
      #endif
        c++;
        break;
      case 104:
      #ifdef CBMC
      __count_3_108++;
      #endif
        c++;
        break;
      case 105:
      #ifdef CBMC
      __count_3_109++;
      #endif
        c++;
        break;
      case 106:
      #ifdef CBMC
      __count_3_110++;
      #endif
        c++;
        break;
      case 107:
      #ifdef CBMC
      __count_111_125++;
      #endif
        c++;
        break;
      case 108:
      #ifdef CBMC
      __count_112_125++;
      #endif
        c++;
        break;
      case 109:
      #ifdef CBMC
      __count_3_113++;
      #endif
        c++;
        break;
      case 110:
      #ifdef CBMC
      __count_114_125++;
      #endif
        c++;
        break;
      case 111:
      #ifdef CBMC
      __count_115_125++;
      #endif
        c++;
        break;
      case 112:
      #ifdef CBMC
      __count_3_116++;
      #endif
        c++;
        break;
      case 113:
      #ifdef CBMC
      __count_3_117++;
      #endif
        c++;
        break;
      case 114:
      #ifdef CBMC
      __count_3_118++;
      #endif
        c++;
        break;
      case 115:
      #ifdef CBMC
      __count_119_125++;
      #endif
        c++;
        break;
      case 116:
      #ifdef CBMC
      __count_120_125++;
      #endif
        c++;
        break;
      case 117:
      #ifdef CBMC
      __count_3_121++;
      #endif
        c++;
        break;
      case 118:
      #ifdef CBMC
      __count_122_125++;
      #endif
        c++;
        break;
      case 119:
      #ifdef CBMC
      __count_123_125++;
      #endif
        c++;
        break;
      default:
      #ifdef CBMC
      __count_124_125++;
      #endif
        c--;
        break;
    }
  }
  
   #ifdef CBMC
   __count_126_127++;
   __count_127++;
   #endif
   
   #ifdef CBMC
assert(__count_126_2  <= 121); // Loop counter property
assert(__count_127 >= 1); // Lower capacity constraint
assert(__count_127 <= 1); // Upper capacity constraint
assert(__count_3_4 >= 1); // Lower capacity constraint
assert(__count_3_4 <= 1); // Upper capacity constraint
assert(__count_3_5 >= 1); // Lower capacity constraint
assert(__count_3_5 <= 1); // Upper capacity constraint
assert(__count_3_6 >= 1); // Lower capacity constraint
assert(__count_3_6 <= 1); // Upper capacity constraint
assert(__count_3_7 >= 1); // Lower capacity constraint
assert(__count_3_7 <= 1); // Upper capacity constraint
assert(__count_3_9 >= 1); // Lower capacity constraint
assert(__count_3_9 <= 1); // Upper capacity constraint
assert(__count_3_12 >= 1); // Lower capacity constraint
assert(__count_3_12 <= 1); // Upper capacity constraint
assert(__count_3_13 >= 1); // Lower capacity constraint
assert(__count_3_13 <= 1); // Upper capacity constraint
assert(__count_3_14 >= 1); // Lower capacity constraint
assert(__count_3_14 <= 1); // Upper capacity constraint
assert(__count_3_17 >= 1); // Lower capacity constraint
assert(__count_3_17 <= 1); // Upper capacity constraint
assert(__count_3_20 >= 1); // Lower capacity constraint
assert(__count_3_20 <= 1); // Upper capacity constraint
assert(__count_3_21 >= 1); // Lower capacity constraint
assert(__count_3_21 <= 1); // Upper capacity constraint
assert(__count_3_22 >= 1); // Lower capacity constraint
assert(__count_3_22 <= 1); // Upper capacity constraint
assert(__count_3_25 >= 1); // Lower capacity constraint
assert(__count_3_25 <= 1); // Upper capacity constraint
assert(__count_3_28 >= 1); // Lower capacity constraint
assert(__count_3_28 <= 1); // Upper capacity constraint
assert(__count_3_29 >= 1); // Lower capacity constraint
assert(__count_3_29 <= 1); // Upper capacity constraint
assert(__count_3_30 >= 1); // Lower capacity constraint
assert(__count_3_30 <= 1); // Upper capacity constraint
assert(__count_3_33 >= 1); // Lower capacity constraint
assert(__count_3_33 <= 1); // Upper capacity constraint
assert(__count_3_36 >= 1); // Lower capacity constraint
assert(__count_3_36 <= 1); // Upper capacity constraint
assert(__count_3_37 >= 1); // Lower capacity constraint
assert(__count_3_37 <= 1); // Upper capacity constraint
assert(__count_3_38 >= 1); // Lower capacity constraint
assert(__count_3_38 <= 1); // Upper capacity constraint
assert(__count_3_41 >= 1); // Lower capacity constraint
assert(__count_3_41 <= 1); // Upper capacity constraint
assert(__count_3_44 >= 1); // Lower capacity constraint
assert(__count_3_44 <= 1); // Upper capacity constraint
assert(__count_3_45 >= 1); // Lower capacity constraint
assert(__count_3_45 <= 1); // Upper capacity constraint
assert(__count_3_46 >= 1); // Lower capacity constraint
assert(__count_3_46 <= 1); // Upper capacity constraint
assert(__count_3_49 >= 1); // Lower capacity constraint
assert(__count_3_49 <= 1); // Upper capacity constraint
assert(__count_3_52 >= 1); // Lower capacity constraint
assert(__count_3_52 <= 1); // Upper capacity constraint
assert(__count_3_53 >= 1); // Lower capacity constraint
assert(__count_3_53 <= 1); // Upper capacity constraint
assert(__count_3_54 >= 1); // Lower capacity constraint
assert(__count_3_54 <= 1); // Upper capacity constraint
assert(__count_3_57 >= 1); // Lower capacity constraint
assert(__count_3_57 <= 1); // Upper capacity constraint
assert(__count_3_60 >= 1); // Lower capacity constraint
assert(__count_3_60 <= 1); // Upper capacity constraint
assert(__count_3_61 >= 1); // Lower capacity constraint
assert(__count_3_61 <= 1); // Upper capacity constraint
assert(__count_3_62 >= 1); // Lower capacity constraint
assert(__count_3_62 <= 1); // Upper capacity constraint
assert(__count_3_65 >= 1); // Lower capacity constraint
assert(__count_3_65 <= 1); // Upper capacity constraint
assert(__count_3_68 >= 1); // Lower capacity constraint
assert(__count_3_68 <= 1); // Upper capacity constraint
assert(__count_3_69 >= 1); // Lower capacity constraint
assert(__count_3_69 <= 1); // Upper capacity constraint
assert(__count_3_70 >= 1); // Lower capacity constraint
assert(__count_3_70 <= 1); // Upper capacity constraint
assert(__count_3_73 >= 1); // Lower capacity constraint
assert(__count_3_73 <= 1); // Upper capacity constraint
assert(__count_3_76 >= 1); // Lower capacity constraint
assert(__count_3_76 <= 1); // Upper capacity constraint
assert(__count_3_77 >= 1); // Lower capacity constraint
assert(__count_3_77 <= 1); // Upper capacity constraint
assert(__count_3_78 >= 1); // Lower capacity constraint
assert(__count_3_78 <= 1); // Upper capacity constraint
assert(__count_3_81 >= 1); // Lower capacity constraint
assert(__count_3_81 <= 1); // Upper capacity constraint
assert(__count_3_84 >= 1); // Lower capacity constraint
assert(__count_3_84 <= 1); // Upper capacity constraint
assert(__count_3_85 >= 1); // Lower capacity constraint
assert(__count_3_85 <= 1); // Upper capacity constraint
assert(__count_3_86 >= 1); // Lower capacity constraint
assert(__count_3_86 <= 1); // Upper capacity constraint
assert(__count_3_89 >= 1); // Lower capacity constraint
assert(__count_3_89 <= 1); // Upper capacity constraint
assert(__count_3_92 >= 1); // Lower capacity constraint
assert(__count_3_92 <= 1); // Upper capacity constraint
assert(__count_3_93 >= 1); // Lower capacity constraint
assert(__count_3_93 <= 1); // Upper capacity constraint
assert(__count_3_94 >= 1); // Lower capacity constraint
assert(__count_3_94 <= 1); // Upper capacity constraint
assert(__count_3_97 >= 1); // Lower capacity constraint
assert(__count_3_97 <= 1); // Upper capacity constraint
assert(__count_3_100 >= 1); // Lower capacity constraint
assert(__count_3_100 <= 1); // Upper capacity constraint
assert(__count_3_101 >= 1); // Lower capacity constraint
assert(__count_3_101 <= 1); // Upper capacity constraint
assert(__count_3_102 >= 1); // Lower capacity constraint
assert(__count_3_102 <= 1); // Upper capacity constraint
assert(__count_3_105 >= 1); // Lower capacity constraint
assert(__count_3_105 <= 1); // Upper capacity constraint
assert(__count_3_108 >= 1); // Lower capacity constraint
assert(__count_3_108 <= 1); // Upper capacity constraint
assert(__count_3_109 >= 1); // Lower capacity constraint
assert(__count_3_109 <= 1); // Upper capacity constraint
assert(__count_3_110 >= 1); // Lower capacity constraint
assert(__count_3_110 <= 1); // Upper capacity constraint
assert(__count_3_113 >= 1); // Lower capacity constraint
assert(__count_3_113 <= 1); // Upper capacity constraint
assert(__count_3_116 >= 1); // Lower capacity constraint
assert(__count_3_116 <= 1); // Upper capacity constraint
assert(__count_3_117 >= 1); // Lower capacity constraint
assert(__count_3_117 <= 1); // Upper capacity constraint
assert(__count_3_118 >= 1); // Lower capacity constraint
assert(__count_3_118 <= 1); // Upper capacity constraint
assert(__count_3_121 >= 1); // Lower capacity constraint
assert(__count_3_121 <= 1); // Upper capacity constraint
assert(__count_8_125 >= 1); // Lower capacity constraint
assert(__count_8_125 <= 1); // Upper capacity constraint
assert(__count_10_125 >= 1); // Lower capacity constraint
assert(__count_10_125 <= 1); // Upper capacity constraint
assert(__count_11_125 >= 1); // Lower capacity constraint
assert(__count_11_125 <= 1); // Upper capacity constraint
assert(__count_15_125 >= 1); // Lower capacity constraint
assert(__count_15_125 <= 1); // Upper capacity constraint
assert(__count_16_125 >= 1); // Lower capacity constraint
assert(__count_16_125 <= 1); // Upper capacity constraint
assert(__count_18_125 >= 1); // Lower capacity constraint
assert(__count_18_125 <= 1); // Upper capacity constraint
assert(__count_19_125 >= 1); // Lower capacity constraint
assert(__count_19_125 <= 1); // Upper capacity constraint
assert(__count_23_125 >= 1); // Lower capacity constraint
assert(__count_23_125 <= 1); // Upper capacity constraint
assert(__count_24_125 >= 1); // Lower capacity constraint
assert(__count_24_125 <= 1); // Upper capacity constraint
assert(__count_26_125 >= 1); // Lower capacity constraint
assert(__count_26_125 <= 1); // Upper capacity constraint
assert(__count_27_125 >= 1); // Lower capacity constraint
assert(__count_27_125 <= 1); // Upper capacity constraint
assert(__count_31_125 >= 1); // Lower capacity constraint
assert(__count_31_125 <= 1); // Upper capacity constraint
assert(__count_32_125 >= 1); // Lower capacity constraint
assert(__count_32_125 <= 1); // Upper capacity constraint
assert(__count_34_125 >= 1); // Lower capacity constraint
assert(__count_34_125 <= 1); // Upper capacity constraint
assert(__count_35_125 >= 1); // Lower capacity constraint
assert(__count_35_125 <= 1); // Upper capacity constraint
assert(__count_39_125 >= 1); // Lower capacity constraint
assert(__count_39_125 <= 1); // Upper capacity constraint
assert(__count_40_125 >= 1); // Lower capacity constraint
assert(__count_40_125 <= 1); // Upper capacity constraint
assert(__count_42_125 >= 1); // Lower capacity constraint
assert(__count_42_125 <= 1); // Upper capacity constraint
assert(__count_43_125 >= 1); // Lower capacity constraint
assert(__count_43_125 <= 1); // Upper capacity constraint
assert(__count_47_125 >= 1); // Lower capacity constraint
assert(__count_47_125 <= 1); // Upper capacity constraint
assert(__count_48_125 >= 1); // Lower capacity constraint
assert(__count_48_125 <= 1); // Upper capacity constraint
assert(__count_50_125 >= 1); // Lower capacity constraint
assert(__count_50_125 <= 1); // Upper capacity constraint
assert(__count_51_125 >= 1); // Lower capacity constraint
assert(__count_51_125 <= 1); // Upper capacity constraint
assert(__count_55_125 >= 1); // Lower capacity constraint
assert(__count_55_125 <= 1); // Upper capacity constraint
assert(__count_56_125 >= 1); // Lower capacity constraint
assert(__count_56_125 <= 1); // Upper capacity constraint
assert(__count_58_125 >= 1); // Lower capacity constraint
assert(__count_58_125 <= 1); // Upper capacity constraint
assert(__count_59_125 >= 1); // Lower capacity constraint
assert(__count_59_125 <= 1); // Upper capacity constraint
assert(__count_63_125 >= 1); // Lower capacity constraint
assert(__count_63_125 <= 1); // Upper capacity constraint
assert(__count_64_125 >= 1); // Lower capacity constraint
assert(__count_64_125 <= 1); // Upper capacity constraint
assert(__count_66_125 >= 1); // Lower capacity constraint
assert(__count_66_125 <= 1); // Upper capacity constraint
assert(__count_67_125 >= 1); // Lower capacity constraint
assert(__count_67_125 <= 1); // Upper capacity constraint
assert(__count_71_125 >= 1); // Lower capacity constraint
assert(__count_71_125 <= 1); // Upper capacity constraint
assert(__count_72_125 >= 1); // Lower capacity constraint
assert(__count_72_125 <= 1); // Upper capacity constraint
assert(__count_74_125 >= 1); // Lower capacity constraint
assert(__count_74_125 <= 1); // Upper capacity constraint
assert(__count_75_125 >= 1); // Lower capacity constraint
assert(__count_75_125 <= 1); // Upper capacity constraint
assert(__count_79_125 >= 1); // Lower capacity constraint
assert(__count_79_125 <= 1); // Upper capacity constraint
assert(__count_80_125 >= 1); // Lower capacity constraint
assert(__count_80_125 <= 1); // Upper capacity constraint
assert(__count_82_125 >= 1); // Lower capacity constraint
assert(__count_82_125 <= 1); // Upper capacity constraint
assert(__count_83_125 >= 1); // Lower capacity constraint
assert(__count_83_125 <= 1); // Upper capacity constraint
assert(__count_87_125 >= 1); // Lower capacity constraint
assert(__count_87_125 <= 1); // Upper capacity constraint
assert(__count_88_125 >= 1); // Lower capacity constraint
assert(__count_88_125 <= 1); // Upper capacity constraint
assert(__count_90_125 >= 1); // Lower capacity constraint
assert(__count_90_125 <= 1); // Upper capacity constraint
assert(__count_91_125 >= 1); // Lower capacity constraint
assert(__count_91_125 <= 1); // Upper capacity constraint
assert(__count_95_125 >= 1); // Lower capacity constraint
assert(__count_95_125 <= 1); // Upper capacity constraint
assert(__count_96_125 >= 1); // Lower capacity constraint
assert(__count_96_125 <= 1); // Upper capacity constraint
assert(__count_98_125 >= 1); // Lower capacity constraint
assert(__count_98_125 <= 1); // Upper capacity constraint
assert(__count_99_125 >= 1); // Lower capacity constraint
assert(__count_99_125 <= 1); // Upper capacity constraint
assert(__count_103_125 >= 1); // Lower capacity constraint
assert(__count_103_125 <= 1); // Upper capacity constraint
assert(__count_104_125 >= 1); // Lower capacity constraint
assert(__count_104_125 <= 1); // Upper capacity constraint
assert(__count_106_125 >= 1); // Lower capacity constraint
assert(__count_106_125 <= 1); // Upper capacity constraint
assert(__count_107_125 >= 1); // Lower capacity constraint
assert(__count_107_125 <= 1); // Upper capacity constraint
assert(__count_111_125 >= 1); // Lower capacity constraint
assert(__count_111_125 <= 1); // Upper capacity constraint
assert(__count_112_125 >= 1); // Lower capacity constraint
assert(__count_112_125 <= 1); // Upper capacity constraint
assert(__count_114_125 >= 1); // Lower capacity constraint
assert(__count_114_125 <= 1); // Upper capacity constraint
assert(__count_115_125 >= 1); // Lower capacity constraint
assert(__count_115_125 <= 1); // Upper capacity constraint
assert(__count_119_125 >= 1); // Lower capacity constraint
assert(__count_119_125 <= 1); // Upper capacity constraint
assert(__count_120_125 >= 1); // Lower capacity constraint
assert(__count_120_125 <= 1); // Upper capacity constraint
assert(__count_122_125 >= 1); // Lower capacity constraint
assert(__count_122_125 <= 1); // Upper capacity constraint
assert(__count_123_125 >= 1); // Lower capacity constraint
assert(__count_123_125 <= 1); // Upper capacity constraint
assert(__count_124_125 == 0); // Dead code
assert(__count_126_127 >= 1); // Lower capacity constraint
assert(__count_126_127 <= 1); // Upper capacity constraint
   #endif
  
  return c;
} 

int 
swi50 (int c)
{
#ifdef CBMC
//==========> swi50 : header 193
int __count_129_191 = 0;
int __count_130_131 = 0;
int __count_130_136 = 0;
int __count_130_137 = 0;
int __count_130_138 = 0;
int __count_130_139 = 0;
int __count_130_144 = 0;
int __count_130_145 = 0;
int __count_130_146 = 0;
int __count_130_147 = 0;
int __count_130_152 = 0;
int __count_130_153 = 0;
int __count_130_154 = 0;
int __count_130_155 = 0;
int __count_130_160 = 0;
int __count_130_161 = 0;
int __count_130_162 = 0;
int __count_130_163 = 0;
int __count_130_168 = 0;
int __count_130_169 = 0;
int __count_130_170 = 0;
int __count_130_171 = 0;
int __count_130_176 = 0;
int __count_130_177 = 0;
int __count_130_178 = 0;
int __count_130_179 = 0;
int __count_130_184 = 0;
int __count_130_185 = 0;
int __count_130_186 = 0;
int __count_130_187 = 0;
int __count_132_192 = 0;
int __count_133_192 = 0;
int __count_134_192 = 0;
int __count_135_192 = 0;
int __count_140_192 = 0;
int __count_141_192 = 0;
int __count_142_192 = 0;
int __count_143_192 = 0;
int __count_148_192 = 0;
int __count_149_192 = 0;
int __count_150_192 = 0;
int __count_151_192 = 0;
int __count_156_192 = 0;
int __count_157_192 = 0;
int __count_158_192 = 0;
int __count_159_192 = 0;
int __count_164_192 = 0;
int __count_165_192 = 0;
int __count_166_192 = 0;
int __count_167_192 = 0;
int __count_172_192 = 0;
int __count_173_192 = 0;
int __count_174_192 = 0;
int __count_175_192 = 0;
int __count_180_192 = 0;
int __count_181_192 = 0;
int __count_182_192 = 0;
int __count_183_192 = 0;
int __count_188_192 = 0;
int __count_189_192 = 0;
int __count_190_192 = 0;
int __count_193_129 = 0; //Loop counter
//==========> swi50 : header 128
int __count_194 = 0;
int __count_193_194 = 0;
#endif

  int i;

  for (i = 0; i < 50; i++)
  {
    #ifdef CBMC
    __count_193_129++;
    #endif  
    switch (i)
    {
      case 0:
      #ifdef CBMC
      __count_130_131++;
      #endif
        c++;
        break;
      case 1:
      #ifdef CBMC
      __count_132_192++;
      #endif
        c++;
        break;
      case 2:
      #ifdef CBMC
      __count_133_192++;
      #endif
        c++;
        break;
      case 3:
      #ifdef CBMC
      __count_134_192++;
      #endif
        c++;
        break;
      case 4:
      #ifdef CBMC
      __count_135_192++;
      #endif
        c++;
        break;
      case 5:
      #ifdef CBMC
      __count_130_136++;
      #endif
        c++;
        break;
      case 6:
      #ifdef CBMC
      __count_130_137++;
      #endif
        c++;
        break;
      case 7:
      #ifdef CBMC
      __count_130_138++;
      #endif
        c++;
        break;
      case 8:
      #ifdef CBMC
      __count_130_139++;
      #endif
        c++;
        break;
      case 9:
      #ifdef CBMC
      __count_140_192++;
      #endif
        c++;
        break;
      case 10:
      #ifdef CBMC
      __count_141_192++;
      #endif
        c++;
        break;
      case 11:
      #ifdef CBMC
      __count_142_192++;
      #endif
        c++;
        break;
      case 12:
      #ifdef CBMC
      __count_143_192++;
      #endif
        c++;
        break;
      case 13:
      #ifdef CBMC
      __count_130_144++;
      #endif
        c++;
        break;
      case 14:
      #ifdef CBMC
      __count_130_145++;
      #endif
        c++;
        break;
      case 15:
      #ifdef CBMC
      __count_130_146++;
      #endif
        c++;
        break;
      case 16:
      #ifdef CBMC
      __count_130_147++;
      #endif
        c++;
        break;
      case 17:
      #ifdef CBMC
      __count_148_192++;
      #endif
        c++;
        break;
      case 18:
      #ifdef CBMC
      __count_149_192++;
      #endif
        c++;
        break;
      case 19:
      #ifdef CBMC
      __count_150_192++;
      #endif
        c++;
        break;
      case 20:
      #ifdef CBMC
      __count_151_192++;
      #endif
        c++;
        break;
      case 21:
      #ifdef CBMC
      __count_130_152++;
      #endif
        c++;
        break;
      case 22:
      #ifdef CBMC
      __count_130_153++;
      #endif
        c++;
        break;
      case 23:
      #ifdef CBMC
      __count_130_154++;
      #endif
        c++;
        break;
      case 24:
      #ifdef CBMC
      __count_130_155++;
      #endif
        c++;
        break;
      case 25:
      #ifdef CBMC
      __count_156_192++;
      #endif
        c++;
        break;
      case 26:
      #ifdef CBMC
      __count_157_192++;
      #endif
        c++;
        break;
      case 27:
      #ifdef CBMC
      __count_158_192++;
      #endif
        c++;
        break;
      case 28:
      #ifdef CBMC
      __count_159_192++;
      #endif
        c++;
        break;
      case 29:
      #ifdef CBMC
      __count_130_160++;
      #endif
        c++;
        break;
      case 30:
      #ifdef CBMC
      __count_130_161++;
      #endif
        c++;
        break;
      case 31:
      #ifdef CBMC
      __count_130_162++;
      #endif
        c++;
        break;
      case 32:
      #ifdef CBMC
      __count_130_163++;
      #endif
        c++;
        break;
      case 33:
      #ifdef CBMC
      __count_164_192++;
      #endif
        c++;
        break;
      case 34:
      #ifdef CBMC
      __count_165_192++;
      #endif
        c++;
        break;
      case 35:
      #ifdef CBMC
      __count_166_192++;
      #endif
        c++;
        break;
      case 36:
      #ifdef CBMC
      __count_167_192++;
      #endif
        c++;
        break;
      case 37:
      #ifdef CBMC
      __count_130_168++;
      #endif
        c++;
        break;
      case 38:
      #ifdef CBMC
      __count_130_169++;
      #endif
        c++;
        break;
      case 39:
      #ifdef CBMC
      __count_130_170++;
      #endif
        c++;
        break;
      case 40:
      #ifdef CBMC
      __count_130_171++;
      #endif
        c++;
        break;
      case 41:
      #ifdef CBMC
      __count_172_192++;
      #endif
        c++;
        break;
      case 42:
      #ifdef CBMC
      __count_173_192++;
      #endif
        c++;
        break;
      case 43:
      #ifdef CBMC
      __count_174_192++;
      #endif
        c++;
        break;
      case 44:
      #ifdef CBMC
      __count_175_192++;
      #endif
        c++;
        break;
      case 45:
      #ifdef CBMC
      __count_130_176++;
      #endif
        c++;
        break;
      case 46:
      #ifdef CBMC
      __count_130_177++;
      #endif
        c++;
        break;
      case 47:
      #ifdef CBMC
      __count_130_178++;
      #endif
        c++;
        break;
      case 48:
      #ifdef CBMC
      __count_130_179++;
      #endif
        c++;
        break;
      case 49:
      #ifdef CBMC
      __count_180_192++;
      #endif
        c++;
        break;
      case 50:
      #ifdef CBMC
      __count_181_192++;
      #endif
        c++;
        break;
      case 51:
      #ifdef CBMC
      __count_182_192++;
      #endif
        c++;
        break;
      case 52:
      #ifdef CBMC
      __count_183_192++;
      #endif
        c++;
        break;
      case 53:
      #ifdef CBMC
      __count_130_184++;
      #endif
        c++;
        break;
      case 54:
      #ifdef CBMC
      __count_130_185++;
      #endif
        c++;
        break;
      case 55:
      #ifdef CBMC
      __count_130_186++;
      #endif
        c++;
        break;
      case 56:
      #ifdef CBMC
      __count_130_187++;
      #endif
        c++;
        break;
      case 57:
      #ifdef CBMC
      __count_188_192++;
      #endif
        c++;
        break;
      case 58:
      #ifdef CBMC
      __count_189_192++;
      #endif
        c++;
        break;
      case 59:
      #ifdef CBMC
      __count_190_192++;
      #endif
        c++;
        break;
      default:
      #ifdef CBMC
      __count_129_191++;
      #endif
        c--;
        break;
    }
  }
  
   #ifdef CBMC
   __count_193_194++;
   __count_194++;
   #endif
   
#ifdef CBMC
assert(__count_193_129  <= 51); // Loop counter property
assert(__count_129_191 == 0); // Dead code
assert(__count_130_131 >= 1); // Lower capacity constraint
assert(__count_130_131 <= 1); // Upper capacity constraint
assert(__count_130_136 >= 1); // Lower capacity constraint
assert(__count_130_136 <= 1); // Upper capacity constraint
assert(__count_130_137 >= 1); // Lower capacity constraint
assert(__count_130_137 <= 1); // Upper capacity constraint
assert(__count_130_138 >= 1); // Lower capacity constraint
assert(__count_130_138 <= 1); // Upper capacity constraint
assert(__count_130_139 >= 1); // Lower capacity constraint
assert(__count_130_139 <= 1); // Upper capacity constraint
assert(__count_133_192 >= 1); // Lower capacity constraint
assert(__count_133_192 <= 1); // Upper capacity constraint
assert(__count_130_144 >= 1); // Lower capacity constraint
assert(__count_130_144 <= 1); // Upper capacity constraint
assert(__count_130_145 >= 1); // Lower capacity constraint
assert(__count_130_145 <= 1); // Upper capacity constraint
assert(__count_130_146 >= 1); // Lower capacity constraint
assert(__count_130_146 <= 1); // Upper capacity constraint
assert(__count_130_147 >= 1); // Lower capacity constraint
assert(__count_130_147 <= 1); // Upper capacity constraint
assert(__count_130_152 >= 1); // Lower capacity constraint
assert(__count_130_152 <= 1); // Upper capacity constraint
assert(__count_130_153 >= 1); // Lower capacity constraint
assert(__count_130_153 <= 1); // Upper capacity constraint
assert(__count_130_154 >= 1); // Lower capacity constraint
assert(__count_130_154 <= 1); // Upper capacity constraint
assert(__count_130_155 >= 1); // Lower capacity constraint
assert(__count_130_155 <= 1); // Upper capacity constraint
assert(__count_130_160 >= 1); // Lower capacity constraint
assert(__count_130_160 <= 1); // Upper capacity constraint
assert(__count_130_161 >= 1); // Lower capacity constraint
assert(__count_130_161 <= 1); // Upper capacity constraint
assert(__count_130_162 >= 1); // Lower capacity constraint
assert(__count_130_162 <= 1); // Upper capacity constraint
assert(__count_130_163 >= 1); // Lower capacity constraint
assert(__count_130_163 <= 1); // Upper capacity constraint
assert(__count_130_168 >= 1); // Lower capacity constraint
assert(__count_130_168 <= 1); // Upper capacity constraint
assert(__count_130_169 >= 1); // Lower capacity constraint
assert(__count_130_169 <= 1); // Upper capacity constraint
assert(__count_130_170 >= 1); // Lower capacity constraint
assert(__count_130_170 <= 1); // Upper capacity constraint
assert(__count_130_171 >= 1); // Lower capacity constraint
assert(__count_130_171 <= 1); // Upper capacity constraint
assert(__count_130_176 >= 1); // Lower capacity constraint
assert(__count_130_176 <= 1); // Upper capacity constraint
assert(__count_130_177 >= 1); // Lower capacity constraint
assert(__count_130_177 <= 1); // Upper capacity constraint
assert(__count_130_178 >= 1); // Lower capacity constraint
assert(__count_130_178 <= 1); // Upper capacity constraint
assert(__count_130_179 >= 1); // Lower capacity constraint
assert(__count_130_179 <= 1); // Upper capacity constraint
assert(__count_130_184 == 0); // Dead code
assert(__count_130_185 == 0); // Dead code
assert(__count_130_186 == 0); // Dead code
assert(__count_130_187 == 0); // Dead code
assert(__count_132_192 >= 1); // Lower capacity constraint
assert(__count_132_192 <= 1); // Upper capacity constraint
assert(__count_194 >= 1); // Lower capacity constraint
assert(__count_194 <= 1); // Upper capacity constraint
assert(__count_134_192 >= 1); // Lower capacity constraint
assert(__count_134_192 <= 1); // Upper capacity constraint
assert(__count_135_192 >= 1); // Lower capacity constraint
assert(__count_135_192 <= 1); // Upper capacity constraint
assert(__count_140_192 >= 1); // Lower capacity constraint
assert(__count_140_192 <= 1); // Upper capacity constraint
assert(__count_141_192 >= 1); // Lower capacity constraint
assert(__count_141_192 <= 1); // Upper capacity constraint
assert(__count_142_192 >= 1); // Lower capacity constraint
assert(__count_142_192 <= 1); // Upper capacity constraint
assert(__count_143_192 >= 1); // Lower capacity constraint
assert(__count_143_192 <= 1); // Upper capacity constraint
assert(__count_148_192 >= 1); // Lower capacity constraint
assert(__count_148_192 <= 1); // Upper capacity constraint
assert(__count_149_192 >= 1); // Lower capacity constraint
assert(__count_149_192 <= 1); // Upper capacity constraint
assert(__count_150_192 >= 1); // Lower capacity constraint
assert(__count_150_192 <= 1); // Upper capacity constraint
assert(__count_151_192 >= 1); // Lower capacity constraint
assert(__count_151_192 <= 1); // Upper capacity constraint
assert(__count_156_192 >= 1); // Lower capacity constraint
assert(__count_156_192 <= 1); // Upper capacity constraint
assert(__count_157_192 >= 1); // Lower capacity constraint
assert(__count_157_192 <= 1); // Upper capacity constraint
assert(__count_158_192 >= 1); // Lower capacity constraint
assert(__count_158_192 <= 1); // Upper capacity constraint
assert(__count_159_192 >= 1); // Lower capacity constraint
assert(__count_159_192 <= 1); // Upper capacity constraint
assert(__count_164_192 >= 1); // Lower capacity constraint
assert(__count_164_192 <= 1); // Upper capacity constraint
assert(__count_165_192 >= 1); // Lower capacity constraint
assert(__count_165_192 <= 1); // Upper capacity constraint
assert(__count_166_192 >= 1); // Lower capacity constraint
assert(__count_166_192 <= 1); // Upper capacity constraint
assert(__count_167_192 >= 1); // Lower capacity constraint
assert(__count_167_192 <= 1); // Upper capacity constraint
assert(__count_172_192 >= 1); // Lower capacity constraint
assert(__count_172_192 <= 1); // Upper capacity constraint
assert(__count_173_192 >= 1); // Lower capacity constraint
assert(__count_173_192 <= 1); // Upper capacity constraint
assert(__count_174_192 >= 1); // Lower capacity constraint
assert(__count_174_192 <= 1); // Upper capacity constraint
assert(__count_175_192 >= 1); // Lower capacity constraint
assert(__count_175_192 <= 1); // Upper capacity constraint
assert(__count_180_192 >= 1); // Lower capacity constraint
assert(__count_180_192 <= 1); // Upper capacity constraint
assert(__count_181_192 == 0); // Dead code
assert(__count_182_192 == 0); // Dead code
assert(__count_183_192 == 0); // Dead code
assert(__count_188_192 == 0); // Dead code
assert(__count_189_192 == 0); // Dead code
assert(__count_190_192 == 0); // Dead code
assert(__count_193_194 >= 1); // Lower capacity constraint
assert(__count_193_194 <= 1); // Upper capacity constraint
#endif
  
  return c;
} 

int 
swi10 (int c)
{
#ifdef CBMC
//==========> swi10 : header 214
int __count_200_212 = 0;
int __count_201_202 = 0;
int __count_201_203 = 0;
int __count_201_204 = 0;
int __count_201_207 = 0;
int __count_201_208 = 0;
int __count_201_209 = 0;
int __count_201_210 = 0;
int __count_201_211 = 0;
int __count_205_213 = 0;
int __count_206_213 = 0;
int __count_214_200 = 0; //Loop counter
//==========> swi10 : header 199
int __count_215 = 0;
int __count_214_215 = 0;
#endif

  int i;
  
  for (i = 0; i < 10; i++)
  {
    #ifdef CBMC
    __count_214_200++;
    #endif
    switch (i)
    {
      case 0:
      #ifdef CBMC
      __count_201_202++;
      #endif
        c++;
        break;
      case 1:
      #ifdef CBMC
      __count_201_203++;
      #endif
        c++;
        break;
      case 2:
      #ifdef CBMC
      __count_201_204++;
      #endif
        c++;
        break;
      case 3:
      #ifdef CBMC
      __count_205_213++;
      #endif
        c++;
        break;
      case 4:
      #ifdef CBMC
      __count_206_213++;
      #endif
        c++;
        break;
      case 5:
      #ifdef CBMC
     __count_201_207++;
      #endif
        c++;
        break;
      case 6:
      #ifdef CBMC
      __count_201_208++;
      #endif
        c++;
        break;
      case 7:
      #ifdef CBMC
     __count_201_209++;
      #endif
        c++;
        break;
      case 8:
      #ifdef CBMC
      __count_201_210++;
      #endif
        c++;
        break;
      case 9:
      #ifdef CBMC
      __count_201_211++;
      #endif
        c++;
        break;
      default:
      #ifdef CBMC
      __count_200_212++; 
      #endif
        c--;
        break;
    }
  }
  
   #ifdef CBMC
   __count_214_215++;
   __count_215++;
   #endif
   
#ifdef CBMC
assert(__count_214_200  <= 11); // Loop counter property
assert(__count_200_212 == 0); // Dead code
assert(__count_201_202 >= 1); // Lower capacity constraint
assert(__count_201_202 <= 1); // Upper capacity constraint
assert(__count_201_203 >= 1); // Lower capacity constraint
assert(__count_201_203 <= 1); // Upper capacity constraint
assert(__count_201_204 >= 1); // Lower capacity constraint
assert(__count_201_204 <= 1); // Upper capacity constraint
assert(__count_201_207 >= 1); // Lower capacity constraint
assert(__count_201_207 <= 1); // Upper capacity constraint
assert(__count_201_208 >= 1); // Lower capacity constraint
assert(__count_201_208 <= 1); // Upper capacity constraint
assert(__count_201_209 >= 1); // Lower capacity constraint
assert(__count_201_209 <= 1); // Upper capacity constraint
assert(__count_201_210 >= 1); // Lower capacity constraint
assert(__count_201_210 <= 1); // Upper capacity constraint
assert(__count_201_211 >= 1); // Lower capacity constraint
assert(__count_201_211 <= 1); // Upper capacity constraint
assert(__count_205_213 >= 1); // Lower capacity constraint
assert(__count_205_213 <= 1); // Upper capacity constraint
assert(__count_206_213 >= 1); // Lower capacity constraint
assert(__count_206_213 <= 1); // Upper capacity constraint
assert(__count_215 >= 1); // Lower capacity constraint
assert(__count_215 <= 1); // Upper capacity constraint
assert(__count_214_215 >= 1); // Lower capacity constraint
assert(__count_214_215 <= 1); // Upper capacity constraint
#endif
  
  return c;
}

int
cover (int c)
{
  c = swi10(c);
  c = swi50(c);
  c = swi120(c);

  return c;
}

int
main (int argc, char *argv[])
{
  volatile int cnt;

  /*
   * One integer value must be supplied
   */
  if (argc != 2)
  {
    return 1;
  }

  cnt = atoi (argv[1]);
  int val = cover(cnt);

  return 0;
}
