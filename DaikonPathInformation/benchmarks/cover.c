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
cover (int c)
{
#ifdef CBMC
//==========> cover 208
int __count_194_206 = 0;
int __count_195_196 = 0;
int __count_195_197 = 0;
int __count_195_198 = 0;
int __count_195_201 = 0;
int __count_195_202 = 0;
int __count_195_203 = 0;
int __count_195_204 = 0;
int __count_195_205 = 0;
int __count_199_207 = 0;
int __count_200_207 = 0;
int __count_208_194 = 0; //Loop counter
//==========> cover 192
int __count_128_190 = 0;
int __count_129_130 = 0;
int __count_129_135 = 0;
int __count_129_136 = 0;
int __count_129_137 = 0;
int __count_129_138 = 0;
int __count_129_143 = 0;
int __count_129_144 = 0;
int __count_129_145 = 0;
int __count_129_146 = 0;
int __count_129_151 = 0;
int __count_129_152 = 0;
int __count_129_153 = 0;
int __count_129_154 = 0;
int __count_129_159 = 0;
int __count_129_160 = 0;
int __count_129_161 = 0;
int __count_129_162 = 0;
int __count_129_167 = 0;
int __count_129_168 = 0;
int __count_129_169 = 0;
int __count_129_170 = 0;
int __count_129_175 = 0;
int __count_129_176 = 0;
int __count_129_177 = 0;
int __count_129_178 = 0;
int __count_129_183 = 0;
int __count_129_184 = 0;
int __count_129_185 = 0;
int __count_129_186 = 0;
int __count_131_191 = 0;
int __count_132_191 = 0;
int __count_133_191 = 0;
int __count_134_191 = 0;
int __count_139_191 = 0;
int __count_140_191 = 0;
int __count_141_191 = 0;
int __count_142_191 = 0;
int __count_147_191 = 0;
int __count_148_191 = 0;
int __count_149_191 = 0;
int __count_150_191 = 0;
int __count_155_191 = 0;
int __count_156_191 = 0;
int __count_157_191 = 0;
int __count_158_191 = 0;
int __count_163_191 = 0;
int __count_164_191 = 0;
int __count_165_191 = 0;
int __count_166_191 = 0;
int __count_171_191 = 0;
int __count_172_191 = 0;
int __count_173_191 = 0;
int __count_174_191 = 0;
int __count_179_191 = 0;
int __count_180_191 = 0;
int __count_181_191 = 0;
int __count_182_191 = 0;
int __count_187_191 = 0;
int __count_188_191 = 0;
int __count_189_191 = 0;
int __count_192_128 = 0; //Loop counter
//==========> cover 126
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
//==========> cover 1
int __count_209 = 0;
int __count_192_193 = 0;
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

  for (i = 0; i < 50; i++)
  {
    #ifdef CBMC
    __count_192_128++;
    #endif  
    switch (i)
    {
      case 0:
      #ifdef CBMC
      __count_129_130++;
      #endif
        c++;
        break;
      case 1:
      #ifdef CBMC
      __count_131_191++;
      #endif
        c++;
        break;
      case 2:
      #ifdef CBMC
      __count_132_191++;
      #endif
        c++;
        break;
      case 3:
      #ifdef CBMC
      __count_133_191++;
      #endif
        c++;
        break;
      case 4:
      #ifdef CBMC
      __count_134_191++;
      #endif
        c++;
        break;
      case 5:
      #ifdef CBMC
      __count_129_135++;
      #endif
        c++;
        break;
      case 6:
      #ifdef CBMC
      __count_129_136++;
      #endif
        c++;
        break;
      case 7:
      #ifdef CBMC
      __count_129_137++;
      #endif
        c++;
        break;
      case 8:
      #ifdef CBMC
      __count_129_138++;
      #endif
        c++;
        break;
      case 9:
      #ifdef CBMC
      __count_139_191++;
      #endif
        c++;
        break;
      case 10:
      #ifdef CBMC
      __count_140_191++;
      #endif
        c++;
        break;
      case 11:
      #ifdef CBMC
      __count_141_191++;
      #endif
        c++;
        break;
      case 12:
      #ifdef CBMC
      __count_142_191++;
      #endif
        c++;
        break;
      case 13:
      #ifdef CBMC
      __count_129_143++;
      #endif
        c++;
        break;
      case 14:
      #ifdef CBMC
       __count_129_144++;
      #endif
        c++;
        break;
      case 15:
      #ifdef CBMC
       __count_129_145++;
      #endif
        c++;
        break;
      case 16:
      #ifdef CBMC
       __count_129_146++;
      #endif
        c++;
        break;
      case 17:
      #ifdef CBMC
      __count_147_191++;
      #endif
        c++;
        break;
      case 18:
      #ifdef CBMC
      __count_148_191++;
      #endif
        c++;
        break;
      case 19:
      #ifdef CBMC
      __count_149_191++;
      #endif
        c++;
        break;
      case 20:
      #ifdef CBMC
      __count_150_191++;
      #endif
        c++;
        break;
      case 21:
      #ifdef CBMC
       __count_129_151++;
      #endif
        c++;
        break;
      case 22:
      #ifdef CBMC
       __count_129_152++;
      #endif
        c++;
        break;
      case 23:
      #ifdef CBMC
       __count_129_153++;
      #endif
        c++;
        break;
      case 24:
      #ifdef CBMC
       __count_129_154++;
      #endif
        c++;
        break;
      case 25:
      #ifdef CBMC
      __count_155_191++;
      #endif
        c++;
        break;
      case 26:
      #ifdef CBMC
      __count_156_191++;
      #endif
        c++;
        break;
      case 27:
      #ifdef CBMC
      __count_157_191++;
      #endif
        c++;
        break;
      case 28:
      #ifdef CBMC
      __count_158_191++;
      #endif
        c++;
        break;
      case 29:
      #ifdef CBMC
       __count_129_159++;
      #endif
        c++;
        break;
      case 30:
      #ifdef CBMC
       __count_129_160++;
      #endif
        c++;
        break;
      case 31:
      #ifdef CBMC
       __count_129_161++;
      #endif
        c++;
        break;
      case 32:
      #ifdef CBMC
       __count_129_162++;
      #endif
        c++;
        break;
      case 33:
      #ifdef CBMC
      __count_163_191++;
      #endif
        c++;
        break;
      case 34:
      #ifdef CBMC
      __count_164_191++;
      #endif
        c++;
        break;
      case 35:
      #ifdef CBMC
      __count_165_191++;
      #endif
        c++;
        break;
      case 36:
      #ifdef CBMC
      __count_166_191++;
      #endif
        c++;
        break;
      case 37:
      #ifdef CBMC
       __count_129_167++;
      #endif
        c++;
        break;
      case 38:
      #ifdef CBMC
       __count_129_168++;
      #endif
        c++;
        break;
      case 39:
      #ifdef CBMC
       __count_129_169++;
      #endif
        c++;
        break;
      case 40:
      #ifdef CBMC
       __count_129_170++;
      #endif
        c++;
        break;
      case 41:
      #ifdef CBMC
      __count_171_191++;
      #endif
        c++;
        break;
      case 42:
      #ifdef CBMC
      __count_172_191++;
      #endif
        c++;
        break;
      case 43:
      #ifdef CBMC
      __count_173_191++;
      #endif
        c++;
        break;
      case 44:
      #ifdef CBMC
      __count_174_191++;
      #endif
        c++;
        break;
      case 45:
      #ifdef CBMC
       __count_129_175++;
      #endif
        c++;
        break;
      case 46:
      #ifdef CBMC
       __count_129_176++;
      #endif
        c++;
        break;
      case 47:
      #ifdef CBMC
       __count_129_177++;
      #endif
        c++;
        break;
      case 48:
      #ifdef CBMC
       __count_129_178++;
      #endif
        c++;
        break;
      case 49:
      #ifdef CBMC
      __count_179_191++;
      #endif
        c++;
        break;
      case 50:
      #ifdef CBMC
      __count_180_191++;
      #endif
        c++;
        break;
      case 51:
      #ifdef CBMC
      __count_181_191++;
      #endif
        c++;
        break;
      case 52:
      #ifdef CBMC
      __count_182_191++;
      #endif
        c++;
        break;
      case 53:
      #ifdef CBMC
      __count_129_183++;
      #endif
        c++;
        break;
      case 54:
      #ifdef CBMC
      __count_129_184++;
      #endif
        c++;
        break;
      case 55:
      #ifdef CBMC
      __count_129_185++;
      #endif
        c++;
        break;
      case 56:
      #ifdef CBMC
      __count_129_186++;
      #endif
        c++;
        break;
      case 57:
      #ifdef CBMC
      __count_187_191++;
      #endif
        c++;
        break;
      case 58:
      #ifdef CBMC
      __count_188_191++;
      #endif
        c++;
        break;
      case 59:
      #ifdef CBMC
      __count_189_191++;
      #endif
        c++;
        break;
      default:
      #ifdef CBMC
      __count_128_190++;
      #endif
        c--;
        break;
    }
  }
  
  #ifdef CBMC
  __count_192_193++; 
  #endif

  for (i = 0; i < 10; i++)
  {
    #ifdef CBMC
    __count_208_194++;
    #endif
    switch (i)
    {
      case 0:
      #ifdef CBMC
      __count_195_196++;
      #endif
        c++;
        break;
      case 1:
      #ifdef CBMC
      __count_195_197++;
      #endif
        c++;
        break;
      case 2:
      #ifdef CBMC
      __count_195_198++;
      #endif
        c++;
        break;
      case 3:
      #ifdef CBMC
      __count_199_207++;
      #endif
        c++;
        break;
      case 4:
      #ifdef CBMC
      __count_200_207++;
      #endif
        c++;
        break;
      case 5:
      #ifdef CBMC
      __count_195_201++;
      #endif
        c++;
        break;
      case 6:
      #ifdef CBMC
      __count_195_202++;
      #endif
        c++;
        break;
      case 7:
      #ifdef CBMC
      __count_195_203++;
      #endif
        c++;
        break;
      case 8:
      #ifdef CBMC
      __count_195_204++;
      #endif
        c++;
        break;
      case 9:
      #ifdef CBMC
      __count_195_205++;
      #endif
        c++;
        break;
      default:
      #ifdef CBMC
      __count_194_206++; 
      #endif
        c--;
        break;
    }
  }
  
  #ifdef CBMC
  __count_209++; 
  #endif
  
  #ifdef CBMC
assert(__count_208_194  <= 11); // Loop counter property
assert(__count_192_128  <= 51); // Loop counter property
assert(__count_126_2  <= 121); // Loop counter property
assert(__count_129_183 == 0); // Dead code
assert(__count_129_184 == 0); // Dead code
assert(__count_129_185 == 0); // Dead code
assert(__count_129_186 == 0); // Dead code
assert(__count_131_191 >= 1); // Lower capacity constraint
assert(__count_131_191 <= 1); // Upper capacity constraint
assert(__count_132_191 >= 1); // Lower capacity constraint
assert(__count_132_191 <= 1); // Upper capacity constraint
assert(__count_133_191 >= 1); // Lower capacity constraint
assert(__count_133_191 <= 1); // Upper capacity constraint
assert(__count_134_191 >= 1); // Lower capacity constraint
assert(__count_134_191 <= 1); // Upper capacity constraint
assert(__count_139_191 >= 1); // Lower capacity constraint
assert(__count_139_191 <= 1); // Upper capacity constraint
assert(__count_140_191 >= 1); // Lower capacity constraint
assert(__count_140_191 <= 1); // Upper capacity constraint
assert(__count_141_191 >= 1); // Lower capacity constraint
assert(__count_141_191 <= 1); // Upper capacity constraint
assert(__count_142_191 >= 1); // Lower capacity constraint
assert(__count_142_191 <= 1); // Upper capacity constraint
assert(__count_147_191 >= 1); // Lower capacity constraint
assert(__count_147_191 <= 1); // Upper capacity constraint
assert(__count_148_191 >= 1); // Lower capacity constraint
assert(__count_148_191 <= 1); // Upper capacity constraint
assert(__count_149_191 >= 1); // Lower capacity constraint
assert(__count_149_191 <= 1); // Upper capacity constraint
assert(__count_150_191 >= 1); // Lower capacity constraint
assert(__count_150_191 <= 1); // Upper capacity constraint
assert(__count_155_191 >= 1); // Lower capacity constraint
assert(__count_155_191 <= 1); // Upper capacity constraint
assert(__count_156_191 >= 1); // Lower capacity constraint
assert(__count_156_191 <= 1); // Upper capacity constraint
assert(__count_157_191 >= 1); // Lower capacity constraint
assert(__count_157_191 <= 1); // Upper capacity constraint
assert(__count_158_191 >= 1); // Lower capacity constraint
assert(__count_158_191 <= 1); // Upper capacity constraint
assert(__count_163_191 >= 1); // Lower capacity constraint
assert(__count_163_191 <= 1); // Upper capacity constraint
assert(__count_164_191 >= 1); // Lower capacity constraint
assert(__count_164_191 <= 1); // Upper capacity constraint
assert(__count_165_191 >= 1); // Lower capacity constraint
assert(__count_165_191 <= 1); // Upper capacity constraint
assert(__count_166_191 >= 1); // Lower capacity constraint
assert(__count_166_191 <= 1); // Upper capacity constraint
assert(__count_171_191 >= 1); // Lower capacity constraint
assert(__count_171_191 <= 1); // Upper capacity constraint
assert(__count_172_191 >= 1); // Lower capacity constraint
assert(__count_172_191 <= 1); // Upper capacity constraint
assert(__count_173_191 >= 1); // Lower capacity constraint
assert(__count_173_191 <= 1); // Upper capacity constraint
assert(__count_174_191 >= 1); // Lower capacity constraint
assert(__count_174_191 <= 1); // Upper capacity constraint
assert(__count_179_191 >= 1); // Lower capacity constraint
assert(__count_179_191 <= 1); // Upper capacity constraint
assert(__count_180_191 == 0); // Dead code
assert(__count_181_191 == 0); // Dead code
assert(__count_182_191 == 0); // Dead code
assert(__count_187_191 == 0); // Dead code
assert(__count_188_191 == 0); // Dead code
assert(__count_189_191 == 0); // Dead code
assert(__count_192_193 >= 1); // Lower capacity constraint
assert(__count_192_193 <= 1); // Upper capacity constraint
assert(__count_194_206 == 0); // Dead code
assert(__count_195_196 >= 1); // Lower capacity constraint
assert(__count_195_196 <= 1); // Upper capacity constraint
assert(__count_195_197 >= 1); // Lower capacity constraint
assert(__count_195_197 <= 1); // Upper capacity constraint
assert(__count_195_198 >= 1); // Lower capacity constraint
assert(__count_195_198 <= 1); // Upper capacity constraint
assert(__count_195_201 >= 1); // Lower capacity constraint
assert(__count_195_201 <= 1); // Upper capacity constraint
assert(__count_195_202 >= 1); // Lower capacity constraint
assert(__count_195_202 <= 1); // Upper capacity constraint
assert(__count_195_203 >= 1); // Lower capacity constraint
assert(__count_195_203 <= 1); // Upper capacity constraint
assert(__count_195_204 >= 1); // Lower capacity constraint
assert(__count_195_204 <= 1); // Upper capacity constraint
assert(__count_195_205 >= 1); // Lower capacity constraint
assert(__count_195_205 <= 1); // Upper capacity constraint
assert(__count_199_207 >= 1); // Lower capacity constraint
assert(__count_199_207 <= 1); // Upper capacity constraint
assert(__count_200_207 >= 1); // Lower capacity constraint
assert(__count_200_207 <= 1); // Upper capacity constraint
assert(__count_209 >= 1); // Lower capacity constraint
assert(__count_209 <= 1); // Upper capacity constraint
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
assert(__count_128_190 == 0); // Dead code
assert(__count_129_130 >= 1); // Lower capacity constraint
assert(__count_129_130 <= 1); // Upper capacity constraint
assert(__count_129_135 >= 1); // Lower capacity constraint
assert(__count_129_135 <= 1); // Upper capacity constraint
assert(__count_129_136 >= 1); // Lower capacity constraint
assert(__count_129_136 <= 1); // Upper capacity constraint
assert(__count_129_137 >= 1); // Lower capacity constraint
assert(__count_129_137 <= 1); // Upper capacity constraint
assert(__count_129_138 >= 1); // Lower capacity constraint
assert(__count_129_138 <= 1); // Upper capacity constraint
assert(__count_129_143 >= 1); // Lower capacity constraint
assert(__count_129_143 <= 1); // Upper capacity constraint
assert(__count_129_144 >= 1); // Lower capacity constraint
assert(__count_129_144 <= 1); // Upper capacity constraint
assert(__count_129_145 >= 1); // Lower capacity constraint
assert(__count_129_145 <= 1); // Upper capacity constraint
assert(__count_129_146 >= 1); // Lower capacity constraint
assert(__count_129_146 <= 1); // Upper capacity constraint
assert(__count_129_151 >= 1); // Lower capacity constraint
assert(__count_129_151 <= 1); // Upper capacity constraint
assert(__count_129_152 >= 1); // Lower capacity constraint
assert(__count_129_152 <= 1); // Upper capacity constraint
assert(__count_129_153 >= 1); // Lower capacity constraint
assert(__count_129_153 <= 1); // Upper capacity constraint
assert(__count_129_154 >= 1); // Lower capacity constraint
assert(__count_129_154 <= 1); // Upper capacity constraint
assert(__count_129_159 >= 1); // Lower capacity constraint
assert(__count_129_159 <= 1); // Upper capacity constraint
assert(__count_129_160 >= 1); // Lower capacity constraint
assert(__count_129_160 <= 1); // Upper capacity constraint
assert(__count_129_161 >= 1); // Lower capacity constraint
assert(__count_129_161 <= 1); // Upper capacity constraint
assert(__count_129_162 >= 1); // Lower capacity constraint
assert(__count_129_162 <= 1); // Upper capacity constraint
assert(__count_129_167 >= 1); // Lower capacity constraint
assert(__count_129_167 <= 1); // Upper capacity constraint
assert(__count_129_168 >= 1); // Lower capacity constraint
assert(__count_129_168 <= 1); // Upper capacity constraint
assert(__count_129_169 >= 1); // Lower capacity constraint
assert(__count_129_169 <= 1); // Upper capacity constraint
assert(__count_129_170 >= 1); // Lower capacity constraint
assert(__count_129_170 <= 1); // Upper capacity constraint
assert(__count_129_175 >= 1); // Lower capacity constraint
assert(__count_129_175 <= 1); // Upper capacity constraint
assert(__count_129_176 >= 1); // Lower capacity constraint
assert(__count_129_176 <= 1); // Upper capacity constraint
assert(__count_129_177 >= 1); // Lower capacity constraint
assert(__count_129_177 <= 1); // Upper capacity constraint
assert(__count_129_178 >= 1); // Lower capacity constraint
assert(__count_129_178 <= 1); // Upper capacity constraint
  #endif
  
  return c;
}

int
main (int argc, char *argv[])
{
  int cnt;

  /*
   * One integer value must be supplied
   */
  if (argc != 2)
  {
    return 1;
  }

  cnt = atoi (argv[1]);
  int val = cover(cnt);
  printf("%d", val);

  return 0;
}
