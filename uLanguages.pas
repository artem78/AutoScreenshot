unit uLanguages;

// Database source: https://github.com/bbqsrc/iso639-databases/blob/master/iso639-lcids.tsv

interface

type
  TIso6391Code = String[2]; // ISO 639-1 Code
  TIso6393Code = String[3]; // ISO 639-3 Code
  TLcid = Cardinal; // Windows Language Code Identifier

function Iso6391FromLcid(ALcid: TLcid): TIso6391Code;
function Iso6393FromLcid(ALcid: TLcid): TIso6393Code;

implementation

uses SysUtils;

type
  TLangCodesRecord = record
    Iso6391Code: TIso6391Code;
    Iso6393Code: TIso6393Code;
    Lcid: TLcid;
  end;

const
  CodesArr: array [0..257] of TLangCodesRecord = (
    (
      Iso6391Code: 'af';
      Iso6393Code: 'afr';
      Lcid: 1078
    ),
    (
      Iso6391Code: 'am';
      Iso6393Code: 'amh';
      Lcid: 1118
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 1025
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 14337
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 15361
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 5121
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 3073
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 2049
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 11265
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 13313
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 12289
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 4097
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 6145
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 8193
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 16385
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 10241
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 7169
    ),
    (
      Iso6391Code: 'ar';
      Iso6393Code: 'ara';
      Lcid: 9217
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'arn';
      Lcid: 1146
    ),
    (
      Iso6391Code: 'as';
      Iso6393Code: 'asm';
      Lcid: 1101
    ),
    (
      Iso6391Code: 'az';
      Iso6393Code: 'aze';
      Lcid: 1068
    ),
    (
      Iso6391Code: 'az';
      Iso6393Code: 'aze';
      Lcid: 2092
    ),
    (
      Iso6391Code: 'ba';
      Iso6393Code: 'bak';
      Lcid: 1133
    ),
    (
      Iso6391Code: 'be';
      Iso6393Code: 'bel';
      Lcid: 1059
    ),
    (
      Iso6391Code: 'bn';
      Iso6393Code: 'ben';
      Lcid: 2117
    ),
    (
      Iso6391Code: 'bn';
      Iso6393Code: 'ben';
      Lcid: 1093
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'bin';
      Lcid: 1126
    ),
    (
      Iso6391Code: 'bo';
      Iso6393Code: 'bod';
      Lcid: 1105
    ),
    (
      Iso6391Code: 'bs';
      Iso6393Code: 'bos';
      Lcid: 5146
    ),
    (
      Iso6391Code: 'bs';
      Iso6393Code: 'bos';
      Lcid: 8218
    ),
    (
      Iso6391Code: 'br';
      Iso6393Code: 'bre';
      Lcid: 1150
    ),
    (
      Iso6391Code: 'bg';
      Iso6393Code: 'bul';
      Lcid: 1026
    ),
    (
      Iso6391Code: 'ca';
      Iso6393Code: 'cat';
      Lcid: 1027
    ),
    (
      Iso6391Code: 'ca';
      Iso6393Code: 'cat';
      Lcid: 2051
    ),
    (
      Iso6391Code: 'cs';
      Iso6393Code: 'ces';
      Lcid: 1029
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'chr';
      Lcid: 1116
    ),
    (
      Iso6391Code: 'co';
      Iso6393Code: 'cos';
      Lcid: 1155
    ),
    (
      Iso6391Code: 'cy';
      Iso6393Code: 'cym';
      Lcid: 1106
    ),
    (
      Iso6391Code: 'da';
      Iso6393Code: 'dan';
      Lcid: 1030
    ),
    (
      Iso6391Code: 'de';
      Iso6393Code: 'deu';
      Lcid: 1031
    ),
    (
      Iso6391Code: 'de';
      Iso6393Code: 'deu';
      Lcid: 3079
    ),
    (
      Iso6391Code: 'de';
      Iso6393Code: 'deu';
      Lcid: 2055
    ),
    (
      Iso6391Code: 'de';
      Iso6393Code: 'deu';
      Lcid: 5127
    ),
    (
      Iso6391Code: 'de';
      Iso6393Code: 'deu';
      Lcid: 4103
    ),
    (
      Iso6391Code: 'dv';
      Iso6393Code: 'div';
      Lcid: 1125
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'dsb';
      Lcid: 2094
    ),
    (
      Iso6391Code: 'dz';
      Iso6393Code: 'dzo';
      Lcid: 3153
    ),
    (
      Iso6391Code: 'el';
      Iso6393Code: 'ell';
      Lcid: 1032
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 1033
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 9225
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 3081
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 10249
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 4105
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 2057
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 15369
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 14345
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 6153
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 16393
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 8201
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 17417
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 5129
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 13321
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 18441
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 11273
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 7177
    ),
    (
      Iso6391Code: 'en';
      Iso6393Code: 'eng';
      Lcid: 12297
    ),
    (
      Iso6391Code: 'et';
      Iso6393Code: 'est';
      Lcid: 1061
    ),
    (
      Iso6391Code: 'eu';
      Iso6393Code: 'eus';
      Lcid: 1069
    ),
    (
      Iso6391Code: 'fo';
      Iso6393Code: 'fao';
      Lcid: 1080
    ),
    (
      Iso6391Code: 'fa';
      Iso6393Code: 'fas';
      Lcid: 1065
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'fil';
      Lcid: 1124
    ),
    (
      Iso6391Code: 'fi';
      Iso6393Code: 'fin';
      Lcid: 1035
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 1036
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 7180
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 2060
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 3084
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 9228
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 4108
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 12300
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 11276
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 15372
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 5132
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 14348
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 6156
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 13324
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 8204
    ),
    (
      Iso6391Code: 'fr';
      Iso6393Code: 'fra';
      Lcid: 10252
    ),
    (
      Iso6391Code: 'fy';
      Iso6393Code: 'fry';
      Lcid: 1122
    ),
    (
      Iso6391Code: 'ff';
      Iso6393Code: 'ful';
      Lcid: 2151
    ),
    (
      Iso6391Code: 'ff';
      Iso6393Code: 'ful';
      Lcid: 1127
    ),
    (
      Iso6391Code: 'gd';
      Iso6393Code: 'gla';
      Lcid: 1169
    ),
    (
      Iso6391Code: 'ga';
      Iso6393Code: 'gle';
      Lcid: 2108
    ),
    (
      Iso6391Code: 'gl';
      Iso6393Code: 'glg';
      Lcid: 1110
    ),
    (
      Iso6391Code: 'gn';
      Iso6393Code: 'grn';
      Lcid: 1140
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'gsw';
      Lcid: 1156
    ),
    (
      Iso6391Code: 'gu';
      Iso6393Code: 'guj';
      Lcid: 1095
    ),
    (
      Iso6391Code: 'ha';
      Iso6393Code: 'hau';
      Lcid: 1128
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'haw';
      Lcid: 1141
    ),
    (
      Iso6391Code: 'he';
      Iso6393Code: 'heb';
      Lcid: 1037
    ),
    (
      Iso6391Code: 'hi';
      Iso6393Code: 'hin';
      Lcid: 1081
    ),
    (
      Iso6391Code: 'hr';
      Iso6393Code: 'hrv';
      Lcid: 1050
    ),
    (
      Iso6391Code: 'hr';
      Iso6393Code: 'hrv';
      Lcid: 4122
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'hsb';
      Lcid: 1070
    ),
    (
      Iso6391Code: 'hu';
      Iso6393Code: 'hun';
      Lcid: 1038
    ),
    (
      Iso6391Code: 'hy';
      Iso6393Code: 'hye';
      Lcid: 1067
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'ibb';
      Lcid: 1129
    ),
    (
      Iso6391Code: 'ig';
      Iso6393Code: 'ibo';
      Lcid: 1136
    ),
    (
      Iso6391Code: 'ii';
      Iso6393Code: 'iii';
      Lcid: 1144
    ),
    (
      Iso6391Code: 'iu';
      Iso6393Code: 'iku';
      Lcid: 2141
    ),
    (
      Iso6391Code: 'iu';
      Iso6393Code: 'iku';
      Lcid: 1117
    ),
    (
      Iso6391Code: 'id';
      Iso6393Code: 'ind';
      Lcid: 1057
    ),
    (
      Iso6391Code: 'is';
      Iso6393Code: 'isl';
      Lcid: 1039
    ),
    (
      Iso6391Code: 'it';
      Iso6393Code: 'ita';
      Lcid: 1040
    ),
    (
      Iso6391Code: 'it';
      Iso6393Code: 'ita';
      Lcid: 2064
    ),
    (
      Iso6391Code: 'ja';
      Iso6393Code: 'jpn';
      Lcid: 1041
    ),
    (
      Iso6391Code: 'kl';
      Iso6393Code: 'kal';
      Lcid: 1135
    ),
    (
      Iso6391Code: 'kn';
      Iso6393Code: 'kan';
      Lcid: 1099
    ),
    (
      Iso6391Code: 'ks';
      Iso6393Code: 'kas';
      Lcid: 2144
    ),
    (
      Iso6391Code: 'ka';
      Iso6393Code: 'kat';
      Lcid: 1079
    ),
    (
      Iso6391Code: 'kr';
      Iso6393Code: 'kau';
      Lcid: 1137
    ),
    (
      Iso6391Code: 'kk';
      Iso6393Code: 'kaz';
      Lcid: 1087
    ),
    (
      Iso6391Code: 'km';
      Iso6393Code: 'khm';
      Lcid: 1107
    ),
    (
      Iso6391Code: 'rw';
      Iso6393Code: 'kin';
      Lcid: 1159
    ),
    (
      Iso6391Code: 'ky';
      Iso6393Code: 'kir';
      Lcid: 1088
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'kok';
      Lcid: 1111
    ),
    (
      Iso6391Code: 'ko';
      Iso6393Code: 'kor';
      Lcid: 1042
    ),
    (
      Iso6391Code: 'ku';
      Iso6393Code: 'kur';
      Lcid: 1170
    ),
    (
      Iso6391Code: 'lo';
      Iso6393Code: 'lao';
      Lcid: 1108
    ),
    (
      Iso6391Code: 'la';
      Iso6393Code: 'lat';
      Lcid: 1142
    ),
    (
      Iso6391Code: 'lv';
      Iso6393Code: 'lav';
      Lcid: 1062
    ),
    (
      Iso6391Code: 'lt';
      Iso6393Code: 'lit';
      Lcid: 1063
    ),
    (
      Iso6391Code: 'lb';
      Iso6393Code: 'ltz';
      Lcid: 1134
    ),
    (
      Iso6391Code: 'ml';
      Iso6393Code: 'mal';
      Lcid: 1100
    ),
    (
      Iso6391Code: 'mr';
      Iso6393Code: 'mar';
      Lcid: 1102
    ),
    (
      Iso6391Code: 'mk';
      Iso6393Code: 'mkd';
      Lcid: 1071
    ),
    (
      Iso6391Code: 'mt';
      Iso6393Code: 'mlt';
      Lcid: 1082
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'mni';
      Lcid: 1112
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'moh';
      Lcid: 1148
    ),
    (
      Iso6391Code: 'mn';
      Iso6393Code: 'mon';
      Lcid: 1104
    ),
    (
      Iso6391Code: 'mn';
      Iso6393Code: 'mon';
      Lcid: 2128
    ),
    (
      Iso6391Code: 'mn';
      Iso6393Code: 'mon';
      Lcid: 3152
    ),
    (
      Iso6391Code: 'mi';
      Iso6393Code: 'mri';
      Lcid: 1153
    ),
    (
      Iso6391Code: 'ms';
      Iso6393Code: 'msa';
      Lcid: 1086
    ),
    (
      Iso6391Code: 'ms';
      Iso6393Code: 'msa';
      Lcid: 2110
    ),
    (
      Iso6391Code: 'my';
      Iso6393Code: 'mya';
      Lcid: 1109
    ),
    (
      Iso6391Code: 'ne';
      Iso6393Code: 'nep';
      Lcid: 1121
    ),
    (
      Iso6391Code: 'ne';
      Iso6393Code: 'nep';
      Lcid: 2145
    ),
    (
      Iso6391Code: 'nl';
      Iso6393Code: 'nld';
      Lcid: 1043
    ),
    (
      Iso6391Code: 'nl';
      Iso6393Code: 'nld';
      Lcid: 2067
    ),
    (
      Iso6391Code: 'nn';
      Iso6393Code: 'nno';
      Lcid: 2068
    ),
    (
      Iso6391Code: 'nb';
      Iso6393Code: 'nob';
      Lcid: 1044
    ),
    (
      Iso6391Code: 'no';
      Iso6393Code: 'nor';
      Lcid: 1044
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'nso';
      Lcid: 1132
    ),
    (
      Iso6391Code: 'oc';
      Iso6393Code: 'oci';
      Lcid: 1154
    ),
    (
      Iso6391Code: 'or';
      Iso6393Code: 'ori';
      Lcid: 1096
    ),
    (
      Iso6391Code: 'om';
      Iso6393Code: 'orm';
      Lcid: 1138
    ),
    (
      Iso6391Code: 'pa';
      Iso6393Code: 'pan';
      Lcid: 1094
    ),
    (
      Iso6391Code: 'pa';
      Iso6393Code: 'pan';
      Lcid: 2118
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'pap';
      Lcid: 1145
    ),
    (
      Iso6391Code: 'pl';
      Iso6393Code: 'pol';
      Lcid: 1045
    ),
    (
      Iso6391Code: 'pt';
      Iso6393Code: 'por';
      Lcid: 1046
    ),
    (
      Iso6391Code: 'pt';
      Iso6393Code: 'por';
      Lcid: 2070
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'prs';
      Lcid: 1164
    ),
    (
      Iso6391Code: 'ps';
      Iso6393Code: 'pus';
      Lcid: 1123
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'quc';
      Lcid: 1158
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'quz';
      Lcid: 1131
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'quz';
      Lcid: 2155
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'quz';
      Lcid: 3179
    ),
    (
      Iso6391Code: 'rm';
      Iso6393Code: 'roh';
      Lcid: 1047
    ),
    (
      Iso6391Code: 'ro';
      Iso6393Code: 'ron';
      Lcid: 1048
    ),
    (
      Iso6391Code: 'ro';
      Iso6393Code: 'ron';
      Lcid: 2072
    ),
    (
      Iso6391Code: 'ru';
      Iso6393Code: 'rus';
      Lcid: 1049
    ),
    (
      Iso6391Code: 'ru';
      Iso6393Code: 'rus';
      Lcid: 2073
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'sah';
      Lcid: 1157
    ),
    (
      Iso6391Code: 'sa';
      Iso6393Code: 'san';
      Lcid: 1103
    ),
    (
      Iso6391Code: 'si';
      Iso6393Code: 'sin';
      Lcid: 1115
    ),
    (
      Iso6391Code: 'sk';
      Iso6393Code: 'slk';
      Lcid: 1051
    ),
    (
      Iso6391Code: 'sl';
      Iso6393Code: 'slv';
      Lcid: 1060
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'sma';
      Lcid: 7227
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'sma';
      Lcid: 6203
    ),
    (
      Iso6391Code: 'se';
      Iso6393Code: 'sme';
      Lcid: 1083
    ),
    (
      Iso6391Code: 'se';
      Iso6393Code: 'sme';
      Lcid: 3131
    ),
    (
      Iso6391Code: 'se';
      Iso6393Code: 'sme';
      Lcid: 2107
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'smj';
      Lcid: 5179
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'smj';
      Lcid: 4155
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'smn';
      Lcid: 9275
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'sms';
      Lcid: 8251
    ),
    (
      Iso6391Code: 'sd';
      Iso6393Code: 'snd';
      Lcid: 2137
    ),
    (
      Iso6391Code: 'sd';
      Iso6393Code: 'snd';
      Lcid: 1113
    ),
    (
      Iso6391Code: 'so';
      Iso6393Code: 'som';
      Lcid: 1143
    ),
    (
      Iso6391Code: 'st';
      Iso6393Code: 'sot';
      Lcid: 1072
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 3082
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 22538
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 11274
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 16394
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 13322
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 9226
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 5130
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 23562
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 7178
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 12298
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 4106
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 18442
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 2058
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 19466
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 6154
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 10250
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 20490
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 15370
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 17418
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 21514
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 14346
    ),
    (
      Iso6391Code: 'es';
      Iso6393Code: 'spa';
      Lcid: 8202
    ),
    (
      Iso6391Code: 'sq';
      Iso6393Code: 'sqi';
      Lcid: 1052
    ),
    (
      Iso6391Code: 'sr';
      Iso6393Code: 'srp';
      Lcid: 9242
    ),
    (
      Iso6391Code: 'sr';
      Iso6393Code: 'srp';
      Lcid: 10266
    ),
    (
      Iso6391Code: 'sr';
      Iso6393Code: 'srp';
      Lcid: 7194
    ),
    (
      Iso6391Code: 'sr';
      Iso6393Code: 'srp';
      Lcid: 12314
    ),
    (
      Iso6391Code: 'sr';
      Iso6393Code: 'srp';
      Lcid: 6170
    ),
    (
      Iso6391Code: 'sr';
      Iso6393Code: 'srp';
      Lcid: 11290
    ),
    (
      Iso6391Code: 'sw';
      Iso6393Code: 'swa';
      Lcid: 1089
    ),
    (
      Iso6391Code: 'sv';
      Iso6393Code: 'swe';
      Lcid: 1053
    ),
    (
      Iso6391Code: 'sv';
      Iso6393Code: 'swe';
      Lcid: 2077
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'syr';
      Lcid: 1114
    ),
    (
      Iso6391Code: 'ta';
      Iso6393Code: 'tam';
      Lcid: 1097
    ),
    (
      Iso6391Code: 'ta';
      Iso6393Code: 'tam';
      Lcid: 2121
    ),
    (
      Iso6391Code: 'tt';
      Iso6393Code: 'tat';
      Lcid: 1092
    ),
    (
      Iso6391Code: 'te';
      Iso6393Code: 'tel';
      Lcid: 1098
    ),
    (
      Iso6391Code: 'tg';
      Iso6393Code: 'tgk';
      Lcid: 1064
    ),
    (
      Iso6391Code: 'th';
      Iso6393Code: 'tha';
      Lcid: 1054
    ),
    (
      Iso6391Code: 'ti';
      Iso6393Code: 'tir';
      Lcid: 2163
    ),
    (
      Iso6391Code: 'ti';
      Iso6393Code: 'tir';
      Lcid: 1139
    ),
    (
      Iso6391Code: 'tn';
      Iso6393Code: 'tsn';
      Lcid: 1074
    ),
    (
      Iso6391Code: 'tn';
      Iso6393Code: 'tsn';
      Lcid: 2098
    ),
    (
      Iso6391Code: 'ts';
      Iso6393Code: 'tso';
      Lcid: 1073
    ),
    (
      Iso6391Code: 'tk';
      Iso6393Code: 'tuk';
      Lcid: 1090
    ),
    (
      Iso6391Code: 'tr';
      Iso6393Code: 'tur';
      Lcid: 1055
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'tzm';
      Lcid: 2143
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'tzm';
      Lcid: 1119
    ),
    (
      Iso6391Code: '';
      Iso6393Code: 'tzm';
      Lcid: 4191
    ),
    (
      Iso6391Code: 'ug';
      Iso6393Code: 'uig';
      Lcid: 1152
    ),
    (
      Iso6391Code: 'uk';
      Iso6393Code: 'ukr';
      Lcid: 1058
    ),
    (
      Iso6391Code: 'ur';
      Iso6393Code: 'urd';
      Lcid: 1056
    ),
    (
      Iso6391Code: 'ur';
      Iso6393Code: 'urd';
      Lcid: 2080
    ),
    (
      Iso6391Code: 'uz';
      Iso6393Code: 'uzb';
      Lcid: 1091
    ),
    (
      Iso6391Code: 'uz';
      Iso6393Code: 'uzb';
      Lcid: 2115
    ),
    (
      Iso6391Code: 've';
      Iso6393Code: 'ven';
      Lcid: 1075
    ),
    (
      Iso6391Code: 'vi';
      Iso6393Code: 'vie';
      Lcid: 1066
    ),
    (
      Iso6391Code: 'wo';
      Iso6393Code: 'wol';
      Lcid: 1160
    ),
    (
      Iso6391Code: 'xh';
      Iso6393Code: 'xho';
      Lcid: 1076
    ),
    (
      Iso6391Code: 'yi';
      Iso6393Code: 'yid';
      Lcid: 1085
    ),
    (
      Iso6391Code: 'yo';
      Iso6393Code: 'yor';
      Lcid: 1130
    ),
    (
      Iso6391Code: 'zh';
      Iso6393Code: 'zho';
      Lcid: 2052
    ),
    (
      Iso6391Code: 'zh';
      Iso6393Code: 'zho';
      Lcid: 3076
    ),
    (
      Iso6391Code: 'zh';
      Iso6393Code: 'zho';
      Lcid: 5124
    ),
    (
      Iso6391Code: 'zh';
      Iso6393Code: 'zho';
      Lcid: 4100
    ),
    (
      Iso6391Code: 'zh';
      Iso6393Code: 'zho';
      Lcid: 1028
    ),
    (
      Iso6391Code: 'zu';
      Iso6393Code: 'zul';
      Lcid: 1077
    )
  );
  

function FindByLcid(ALcid: TLcid): TLangCodesRecord;
var
  I: integer;
begin
  for I := Low(CodesArr) to High(CodesArr) do
  begin
    if CodesArr[I].Lcid = ALcid then
    begin
      Result := CodesArr[I];
      Exit;
    end;
  end;

  raise Exception.Create('LCID not found');
end;

function Iso6391FromLcid(ALcid: TLcid): TIso6391Code;
begin
  Result := '';
  try
    Result := FindByLcid(ALcid).Iso6391Code;
  except
  end;
end;

function Iso6393FromLcid(ALcid: TLcid): TIso6393Code;
begin
  Result := '';
  try
    Result := FindByLcid(ALcid).Iso6393Code;
  except
  end;
end;

end.
 