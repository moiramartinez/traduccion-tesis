capture program drop MANUFACTURING
program define MANUFACTURING

    version 11.1

    syntax [if/], NEWvar(varname) OLDvar(varname)

    if "`if'"!="" local if "(`if') & "
	
qui {
replace `newvar'= 	1	 if `if'`oldvar'==	11
replace `newvar'= 	1	 if `if'`oldvar'==	12
replace `newvar'= 	1	 if `if'`oldvar'==	13
replace `newvar'= 	1	 if `if'`oldvar'==	14
replace `newvar'= 	0	 if `if'`oldvar'==	15
replace `newvar'= 	0	 if `if'`oldvar'==	20
replace `newvar'= 	0	 if `if'`oldvar'==	50
replace `newvar'= 	1	 if `if'`oldvar'==	101
replace `newvar'= 	1	 if `if'`oldvar'==	102
replace `newvar'= 	1	 if `if'`oldvar'==	103
replace `newvar'= 	1	 if `if'`oldvar'==	111
replace `newvar'= 	1	 if `if'`oldvar'==	112
replace `newvar'= 	1	 if `if'`oldvar'==	120
replace `newvar'= 	1	 if `if'`oldvar'==	131
replace `newvar'= 	1	 if `if'`oldvar'==	132
replace `newvar'= 	1	 if `if'`oldvar'==	141
replace `newvar'= 	1	 if `if'`oldvar'==	142
replace `newvar'= 	1	 if `if'`oldvar'==	143
replace `newvar'= 	1	 if `if'`oldvar'==	144
replace `newvar'= 	1	 if `if'`oldvar'==	145
replace `newvar'= 	1	 if `if'`oldvar'==	151
replace `newvar'= 	1	 if `if'`oldvar'==	152
replace `newvar'= 	1	 if `if'`oldvar'==	153
replace `newvar'= 	1	 if `if'`oldvar'==	154
replace `newvar'= 	1	 if `if'`oldvar'==	155
replace `newvar'= 	1	 if `if'`oldvar'==	156
replace `newvar'= 	1	 if `if'`oldvar'==	157
replace `newvar'= 	1	 if `if'`oldvar'==	158
replace `newvar'= 	1	 if `if'`oldvar'==	159
replace `newvar'= 	1	 if `if'`oldvar'==	160
replace `newvar'= 	1	 if `if'`oldvar'==	171
replace `newvar'= 	1	 if `if'`oldvar'==	172
replace `newvar'= 	1	 if `if'`oldvar'==	173
replace `newvar'= 	1	 if `if'`oldvar'==	174
replace `newvar'= 	1	 if `if'`oldvar'==	175
replace `newvar'= 	1	 if `if'`oldvar'==	176
replace `newvar'= 	1	 if `if'`oldvar'==	177
replace `newvar'= 	1	 if `if'`oldvar'==	181
replace `newvar'= 	1	 if `if'`oldvar'==	182
replace `newvar'= 	1	 if `if'`oldvar'==	183
replace `newvar'= 	1	 if `if'`oldvar'==	191
replace `newvar'= 	1	 if `if'`oldvar'==	192
replace `newvar'= 	1	 if `if'`oldvar'==	193
replace `newvar'= 	1	 if `if'`oldvar'==	201
replace `newvar'= 	1	 if `if'`oldvar'==	202
replace `newvar'= 	1	 if `if'`oldvar'==	203
replace `newvar'= 	1	 if `if'`oldvar'==	204
replace `newvar'= 	1	 if `if'`oldvar'==	205
replace `newvar'= 	1	 if `if'`oldvar'==	211
replace `newvar'= 	1	 if `if'`oldvar'==	212
replace `newvar'= 	1	 if `if'`oldvar'==	221
replace `newvar'= 	1	 if `if'`oldvar'==	222
replace `newvar'= 	1	 if `if'`oldvar'==	223
replace `newvar'= 	1	 if `if'`oldvar'==	231
replace `newvar'= 	1	 if `if'`oldvar'==	232
replace `newvar'= 	1	 if `if'`oldvar'==	233
replace `newvar'= 	1	 if `if'`oldvar'==	241
replace `newvar'= 	1	 if `if'`oldvar'==	242
replace `newvar'= 	1	 if `if'`oldvar'==	243
replace `newvar'= 	1	 if `if'`oldvar'==	244
replace `newvar'= 	1	 if `if'`oldvar'==	245
replace `newvar'= 	1	 if `if'`oldvar'==	246
replace `newvar'= 	1	 if `if'`oldvar'==	247
replace `newvar'= 	1	 if `if'`oldvar'==	251
replace `newvar'= 	1	 if `if'`oldvar'==	252
replace `newvar'= 	1	 if `if'`oldvar'==	261
replace `newvar'= 	1	 if `if'`oldvar'==	262
replace `newvar'= 	1	 if `if'`oldvar'==	263
replace `newvar'= 	1	 if `if'`oldvar'==	264
replace `newvar'= 	1	 if `if'`oldvar'==	265
replace `newvar'= 	1	 if `if'`oldvar'==	266
replace `newvar'= 	1	 if `if'`oldvar'==	267
replace `newvar'= 	1	 if `if'`oldvar'==	268
replace `newvar'= 	1	 if `if'`oldvar'==	271
replace `newvar'= 	1	 if `if'`oldvar'==	272
replace `newvar'= 	1	 if `if'`oldvar'==	273
replace `newvar'= 	1	 if `if'`oldvar'==	274
replace `newvar'= 	1	 if `if'`oldvar'==	275
replace `newvar'= 	1	 if `if'`oldvar'==	281
replace `newvar'= 	1	 if `if'`oldvar'==	282
replace `newvar'= 	1	 if `if'`oldvar'==	283
replace `newvar'= 	1	 if `if'`oldvar'==	284
replace `newvar'= 	1	 if `if'`oldvar'==	285
replace `newvar'= 	1	 if `if'`oldvar'==	286
replace `newvar'= 	1	 if `if'`oldvar'==	287
replace `newvar'= 	1	 if `if'`oldvar'==	291
replace `newvar'= 	1	 if `if'`oldvar'==	292
replace `newvar'= 	1	 if `if'`oldvar'==	293
replace `newvar'= 	1	 if `if'`oldvar'==	294
replace `newvar'= 	1	 if `if'`oldvar'==	295
replace `newvar'= 	1	 if `if'`oldvar'==	296
replace `newvar'= 	1	 if `if'`oldvar'==	297
replace `newvar'= 	1	 if `if'`oldvar'==	300
replace `newvar'= 	1	 if `if'`oldvar'==	311
replace `newvar'= 	1	 if `if'`oldvar'==	312
replace `newvar'= 	1	 if `if'`oldvar'==	313
replace `newvar'= 	1	 if `if'`oldvar'==	314
replace `newvar'= 	1	 if `if'`oldvar'==	315
replace `newvar'= 	1	 if `if'`oldvar'==	316
replace `newvar'= 	1	 if `if'`oldvar'==	321
replace `newvar'= 	1	 if `if'`oldvar'==	322
replace `newvar'= 	1	 if `if'`oldvar'==	323
replace `newvar'= 	1	 if `if'`oldvar'==	331
replace `newvar'= 	1	 if `if'`oldvar'==	332
replace `newvar'= 	1	 if `if'`oldvar'==	333
replace `newvar'= 	1	 if `if'`oldvar'==	334
replace `newvar'= 	1	 if `if'`oldvar'==	335
replace `newvar'= 	1	 if `if'`oldvar'==	341
replace `newvar'= 	1	 if `if'`oldvar'==	342
replace `newvar'= 	1	 if `if'`oldvar'==	343
replace `newvar'= 	1	 if `if'`oldvar'==	351
replace `newvar'= 	1	 if `if'`oldvar'==	352
replace `newvar'= 	1	 if `if'`oldvar'==	353
replace `newvar'= 	1	 if `if'`oldvar'==	354
replace `newvar'= 	1	 if `if'`oldvar'==	355
replace `newvar'= 	1	 if `if'`oldvar'==	361
replace `newvar'= 	1	 if `if'`oldvar'==	362
replace `newvar'= 	1	 if `if'`oldvar'==	363
replace `newvar'= 	1	 if `if'`oldvar'==	364
replace `newvar'= 	1	 if `if'`oldvar'==	365
replace `newvar'= 	1	 if `if'`oldvar'==	366
replace `newvar'= 	0	 if `if'`oldvar'==	371
replace `newvar'= 	0	 if `if'`oldvar'==	372
replace `newvar'= 	0	 if `if'`oldvar'==	401
replace `newvar'= 	0	 if `if'`oldvar'==	402
replace `newvar'= 	0	 if `if'`oldvar'==	403
replace `newvar'= 	0	 if `if'`oldvar'==	410
replace `newvar'= 	0	 if `if'`oldvar'==	451
replace `newvar'= 	0	 if `if'`oldvar'==	452
replace `newvar'= 	0	 if `if'`oldvar'==	453
replace `newvar'= 	0	 if `if'`oldvar'==	454
replace `newvar'= 	0	 if `if'`oldvar'==	455
replace `newvar'= 	0	 if `if'`oldvar'==	501
replace `newvar'= 	0	 if `if'`oldvar'==	502
replace `newvar'= 	0	 if `if'`oldvar'==	503
replace `newvar'= 	0	 if `if'`oldvar'==	504
replace `newvar'= 	0	 if `if'`oldvar'==	505
replace `newvar'= 	0	 if `if'`oldvar'==	511
replace `newvar'= 	0	 if `if'`oldvar'==	512
replace `newvar'= 	0	 if `if'`oldvar'==	513
replace `newvar'= 	0	 if `if'`oldvar'==	514
replace `newvar'= 	0	 if `if'`oldvar'==	515
replace `newvar'= 	0	 if `if'`oldvar'==	516
replace `newvar'= 	0	 if `if'`oldvar'==	517
replace `newvar'= 	0	 if `if'`oldvar'==	521
replace `newvar'= 	0	 if `if'`oldvar'==	522
replace `newvar'= 	0	 if `if'`oldvar'==	523
replace `newvar'= 	0	 if `if'`oldvar'==	524
replace `newvar'= 	0	 if `if'`oldvar'==	525
replace `newvar'= 	0	 if `if'`oldvar'==	526
replace `newvar'= 	0	 if `if'`oldvar'==	527
replace `newvar'= 	0	 if `if'`oldvar'==	551
replace `newvar'= 	0	 if `if'`oldvar'==	552
replace `newvar'= 	0	 if `if'`oldvar'==	553
replace `newvar'= 	0	 if `if'`oldvar'==	554
replace `newvar'= 	0	 if `if'`oldvar'==	555
replace `newvar'= 	0	 if `if'`oldvar'==	601
replace `newvar'= 	0	 if `if'`oldvar'==	602
replace `newvar'= 	0	 if `if'`oldvar'==	603
replace `newvar'= 	0	 if `if'`oldvar'==	611
replace `newvar'= 	0	 if `if'`oldvar'==	612
replace `newvar'= 	0	 if `if'`oldvar'==	621
replace `newvar'= 	0	 if `if'`oldvar'==	622
replace `newvar'= 	0	 if `if'`oldvar'==	623
replace `newvar'= 	0	 if `if'`oldvar'==	631
replace `newvar'= 	0	 if `if'`oldvar'==	632
replace `newvar'= 	0	 if `if'`oldvar'==	633
replace `newvar'= 	0	 if `if'`oldvar'==	634
replace `newvar'= 	0	 if `if'`oldvar'==	641
replace `newvar'= 	0	 if `if'`oldvar'==	642
replace `newvar'= 	0	 if `if'`oldvar'==	651
replace `newvar'= 	0	 if `if'`oldvar'==	652
replace `newvar'= 	0	 if `if'`oldvar'==	660
replace `newvar'= 	0	 if `if'`oldvar'==	671
replace `newvar'= 	0	 if `if'`oldvar'==	672
replace `newvar'= 	0	 if `if'`oldvar'==	701
replace `newvar'= 	0	 if `if'`oldvar'==	702
replace `newvar'= 	0	 if `if'`oldvar'==	703
replace `newvar'= 	0	 if `if'`oldvar'==	711
replace `newvar'= 	0	 if `if'`oldvar'==	712
replace `newvar'= 	0	 if `if'`oldvar'==	713
replace `newvar'= 	0	 if `if'`oldvar'==	714
replace `newvar'= 	0	 if `if'`oldvar'==	721
replace `newvar'= 	0	 if `if'`oldvar'==	722
replace `newvar'= 	0	 if `if'`oldvar'==	723
replace `newvar'= 	0	 if `if'`oldvar'==	724
replace `newvar'= 	0	 if `if'`oldvar'==	725
replace `newvar'= 	0	 if `if'`oldvar'==	726
replace `newvar'= 	0	 if `if'`oldvar'==	731
replace `newvar'= 	0	 if `if'`oldvar'==	732
replace `newvar'= 	0	 if `if'`oldvar'==	741
replace `newvar'= 	0	 if `if'`oldvar'==	742
replace `newvar'= 	0	 if `if'`oldvar'==	743
replace `newvar'= 	0	 if `if'`oldvar'==	744
replace `newvar'= 	0	 if `if'`oldvar'==	745
replace `newvar'= 	0	 if `if'`oldvar'==	746
replace `newvar'= 	0	 if `if'`oldvar'==	747
replace `newvar'= 	0	 if `if'`oldvar'==	748
replace `newvar'= 	0	 if `if'`oldvar'==	751
replace `newvar'= 	0	 if `if'`oldvar'==	752
replace `newvar'= 	0	 if `if'`oldvar'==	753
replace `newvar'= 	0	 if `if'`oldvar'==	801
replace `newvar'= 	0	 if `if'`oldvar'==	802
replace `newvar'= 	0	 if `if'`oldvar'==	803
replace `newvar'= 	0	 if `if'`oldvar'==	804
replace `newvar'= 	0	 if `if'`oldvar'==	851
replace `newvar'= 	0	 if `if'`oldvar'==	852
replace `newvar'= 	0	 if `if'`oldvar'==	853
replace `newvar'= 	0	 if `if'`oldvar'==	900
replace `newvar'= 	0	 if `if'`oldvar'==	911
replace `newvar'= 	0	 if `if'`oldvar'==	912
replace `newvar'= 	0	 if `if'`oldvar'==	913
replace `newvar'= 	0	 if `if'`oldvar'==	921
replace `newvar'= 	0	 if `if'`oldvar'==	922
replace `newvar'= 	0	 if `if'`oldvar'==	923
replace `newvar'= 	0	 if `if'`oldvar'==	924
replace `newvar'= 	0	 if `if'`oldvar'==	925
replace `newvar'= 	0	 if `if'`oldvar'==	926
replace `newvar'= 	0	 if `if'`oldvar'==	927
replace `newvar'= 	0	 if `if'`oldvar'==	930
replace `newvar'= 	0	 if `if'`oldvar'==	950
replace `newvar'= 	0	 if `if'`oldvar'==	990


}

end

exit