var searchIndex = new Map(JSON.parse('[\
["cont_arith_code",{"t":"FFFFIKFFKKNNNNNNNNNNNNOONNOOCNNNNNNNNNNNNOMONNNNNNNNNNNNNNNOMOONNNNNNMNNMNNMNOOONNNNNNNNNNNNMMNNNNNNNNNNNNNNNNNNONNNNNNCCFFNNNNNNNNONNNNNNNNNNNNNNNNNNNNONNNNONNNNNNNNNNNNNNNNNNNNNNNNFFFFONNNNNNNNNNNNNNNNNNNNOONOONNNNHNNNNNNNNNNNNONONNNNNNNNHNONOHNHNHNNONNNNNNNNNNHNNNNNNHNNHOOOONNNNNNNNNNNNNNNNNNNNNNNNNNNNOONNNN","n":["Decoder","Decoder8","Encoder","Encoder8","Index","Model","Sample","Samples","TruncatedDistribution","UnivariateDistribution","borrow","borrow","borrow","borrow","borrow","borrow","borrow_mut","borrow_mut","borrow_mut","borrow_mut","borrow_mut","borrow_mut","code","count","decode","decode","distr","distr","distribution","from","from","from","from","from","from","from_subset","from_subset","from_subset","from_subset","from_subset","from_subset","head","hi","hi_splits","into","into","into","into","into","into","into_iter","into_iter","is_in_subset","is_in_subset","is_in_subset","is_in_subset","is_in_subset","is_in_subset","is_resolved","last_bit","lo","lo_splits","model","new","new","new","new","next","next","next_distr","next_distr","next_distr","push","push","push","quantile","repeatedly","resolve","sampl","tail","to_subset","to_subset","to_subset","to_subset","to_subset","to_subset","to_subset_unchecked","to_subset_unchecked","to_subset_unchecked","to_subset_unchecked","to_subset_unchecked","to_subset_unchecked","truncate","truncated","try_from","try_from","try_from","try_from","try_from","try_from","try_into","try_into","try_into","try_into","try_into","try_into","type_id","type_id","type_id","type_id","type_id","type_id","vec","vzip","vzip","vzip","vzip","vzip","vzip","categorical","gaussian","Categorical","TruncatedCategorical","borrow","borrow","borrow_mut","borrow_mut","clone","clone","clone_into","clone_into","count","entropy","eq","eq","fmt","fmt","from","from","from_data","from_subset","from_subset","get_key","hi","index_of","into","into","is_in_subset","is_in_subset","is_resolved","kld","len","ln_ps","lo","log_pmf","log_probability","lookup","map","new","next_distr","normalize","partial_cmp","partial_cmp","push","quantile","singleton","to_owned","to_owned","to_subset","to_subset","to_subset_unchecked","to_subset_unchecked","truncate","truncated","try_from","try_from","try_into","try_into","type_id","type_id","vzip","vzip","Gaussian","TruncatedGaussian","WithReplacement","WithoutReplacement","bins","bits","bits","borrow","borrow","borrow","borrow","borrow_mut","borrow_mut","borrow_mut","borrow_mut","cdf","clone","clone","clone","clone","clone_into","clone_into","clone_into","clone_into","count","count","ddof","definite_integral","distr","distr","eq","eq","eq","eq","floor_rem","fmt","fmt","fmt","fmt","from","from","from","from","from_subset","from_subset","from_subset","from_subset","gaussian","hi","hi","into","into","into","into","is_in_subset","is_in_subset","is_in_subset","is_in_subset","lerp","lerp","ln_prob","lo","lo","log_cdf","log_cdf","log_pdf","log_pdf","log_probability","log_probability","log_survival","mean","mle","mle","mle","next_distr","next_distr","next_distr","partial_cmp","partial_cmp","partial_cmp","partial_cmp","pdf","pdf","push","push","push","q_cdf","q_quantile","quantile","quantile","quantile","quantile_exp","s0","s1","s2","stdev","to_owned","to_owned","to_owned","to_owned","to_subset","to_subset","to_subset","to_subset","to_subset_unchecked","to_subset_unchecked","to_subset_unchecked","to_subset_unchecked","translate","truncate","truncated","try_from","try_from","try_from","try_from","try_into","try_into","try_into","try_into","type_id","type_id","type_id","type_id","variance","vec","vec","vzip","vzip","vzip","vzip"],"q":[[0,"cont_arith_code"],[119,"cont_arith_code::distribution"],[121,"cont_arith_code::distribution::categorical"],[182,"cont_arith_code::distribution::gaussian"],[312,"core::iter::traits::iterator"],[313,"core::option"],[314,"alloc::boxed"],[315,"core::clone"],[316,"alloc::vec"],[317,"core::result"],[318,"core::any"],[319,"core::cmp"],[320,"core::fmt"],[321,"core::iter::traits::collect"]],"i":[0,0,0,0,0,0,0,0,0,0,15,17,10,11,1,6,15,17,10,11,1,6,1,17,1,6,15,1,0,15,17,10,11,1,6,15,17,10,11,1,6,10,8,1,15,17,10,11,1,6,10,11,15,17,10,11,1,6,8,1,8,1,1,10,11,1,6,10,11,2,15,17,2,15,17,8,17,15,17,10,15,17,10,11,1,6,15,17,10,11,1,6,8,13,15,17,10,11,1,6,15,17,10,11,1,6,15,17,10,11,1,6,17,15,17,10,11,1,6,0,0,0,0,25,26,25,26,25,26,25,26,25,25,25,26,25,26,25,26,25,25,26,25,26,25,25,26,25,26,26,25,25,26,26,25,25,25,25,26,25,26,25,26,25,26,25,25,26,25,26,25,26,26,25,25,26,25,26,25,26,25,26,0,0,0,0,37,36,37,36,37,38,39,36,37,38,39,36,36,37,38,39,36,37,38,39,39,38,36,36,38,39,36,37,38,39,0,36,37,38,39,36,37,38,39,36,37,38,39,37,37,37,36,37,38,39,36,37,38,39,0,36,37,37,37,0,36,0,36,0,36,36,36,36,38,39,36,38,39,36,37,38,39,0,36,36,38,39,36,36,0,36,37,0,36,36,36,36,36,37,38,39,36,37,38,39,36,37,38,39,36,37,36,36,37,38,39,36,37,38,39,36,37,38,39,36,38,39,36,37,38,39],"f":"``````````{ce{}{}}00000000000``{{{b{egc}}}c{}{{d{c}}}{{j{}{{f{h}}}}}}{{{l{egc}}}c{}{{d{c}}}{{j{}{{f{n}}}}}}```{cc{}}00000333333`{A`Ab}`44444444{ch{}}00000{A`h}`2``{{eg}Ad{}{{d{c}}}{{j{}{{f{Ab}}}}}}{{eg}Af{}{{d{c}}}{{j{}{{f{Ab}}}}}}{{eg}{{b{egc}}}{}{{d{c}}}{{j{}{{f{h}}}}}}{{eg}{{l{egc}}}{}{{d{c}}}{{j{}{{f{n}}}}}}{Ad{{Ah{h}}}}{Af{{Ah{c}}}{}}{d{{Al{Aj}}}}{{{An{ce}}}{{Al{Aj}}}{AjB`}{}}{{{Bb{ce}}}{{Al{Aj}}}{AjB`}{}}{{dAb}{{Ah{c}}}{}}{{{An{ce}}Ab}{{Ah{e}}}{AjB`}{}}{{{Bb{ce}}Ab}{{Ah{{Bd{e}}}}}{AjB`}{}}{{A`Bf}{{Bh{AbBf}}}}{{{An{ce}}Bj}{{Bb{ce}}}{}{}}```{c{{Ah{e}}}{}{}}00000{ce{}{}}00000{{A`BfAbBfh}Bl}{Aj{{Al{A`}}}}{c{{Bn{e}}}{}{}}00000000000{cC`{}}00000`444444````4444{{{Cb{c}}}{{Cb{c}}}B`}{CdCd}{{ce}Bl{}{}}0`{{{Cb{c}}}Bf{}}{{{Cb{c}}{Cb{c}}}hCf}{{CdCd}h}{{{Cb{c}}Ch}CjCl}{{CdCh}Cj}{cc{}}0{e{{Cb{c}}}Cn{{D`{}{{f{c}}}}}}>>{{{Cb{c}}Bj}c{}}{CdDb}{{{Cb{c}}c}{{Ah{Bj}}}{B`Cn}}{ce{}{}}0{ch{}}0{Cdh}{{{Cb{c}}{Cb{c}}}Bf{B`Cn}}{{{Cb{c}}}Bj{}}`6{{{Cb{c}}c}Bf{B`Cn}}0{{{Cb{c}}c}{{Ah{Bf}}}{B`Cn}}`{{{Bd{Bf}}}Cd}{{{Cb{c}}}{{Al{Aj}}}{B`CnCl}}{CdBl}{{{Cb{c}}{Cb{c}}}{{Ah{Dd}}}Df}{{CdCd}{{Ah{Dd}}}}{{{Cb{c}}Db}{{Ah{c}}}{B`CnCl}}{{CdBf}{{Bh{DbBf}}}}{c{{Cb{c}}}{}}>>{c{{Ah{e}}}{}{}}0??{{CdBfDbBfh}Bl}{{{Cb{c}}}{{Al{A`}}}Cl}{c{{Bn{e}}}{}{}}000{cC`{}}0{ce{}{}}0`````{{DhAb}Bf}{{DjAb}Bf}22222222{{DhBf}Bf}{DhDh}{DjDj}{DlDl}{DnDn}{{ce}Bl{}{}}000{DnBj}``{{DhBfBf}Bf}``{{DhDh}h}{{DjDj}h}{{DlDl}h}{{DnDn}h}{Bf{{Bh{DbBf}}}}{{DhCh}Cj}{{DjCh}Cj}{{DlCh}Cj}{{DnCh}Cj}{cc{}}000{ce{}{}}000`{DjDb}`1111{ch{}}000{{BfBfBf}Bf}{{DhBfBfBf}Bf}`3`{BfBf}{{DhBf}Bf}10{{BfBf}Bf}{{DhBfBf}Bf}2`{{cn}Dh{{j{}{{f{Db}}}}}}{cDl{{j{}{{f{Db}}}}}}{cDn{{j{}{{f{Db}}}}}}{Dh{{Al{Aj}}}}{Dl{{Al{Aj}}}}{Dn{{Al{Aj}}}}{{DhDh}{{Ah{Dd}}}}{{DjDj}{{Ah{Dd}}}}{{DlDl}{{Ah{Dd}}}}{{DnDn}{{Ah{Dd}}}}=<{{DhDb}{{Ah{Db}}}}{{DlDb}{{Ah{{Bd{Db}}}}}}{{DnDb}{{Ah{{Bd{Db}}}}}}{{DhBjBf}Bj}{{DhBjBj}Bf}{BfBf}{{DhBf}Bf}{{DjBf}{{Bh{DbBf}}}}2````{ce{}{}}000{c{{Ah{e}}}{}{}}0001111{{DhBf}Dh}{{DjBfDbBfh}Bl}{Dh{{Al{A`}}}}{c{{Bn{e}}}{}{}}0000000{cC`{}}000{DhBf}``7777","D":"Jn","p":[[5,"Decoder",0],[10,"Model",0],[17,"Item"],[1,"bool"],[10,"Iterator",312],[5,"Decoder8",0],[1,"u8"],[10,"TruncatedDistribution",0],[8,"Index",0],[5,"Encoder",0],[5,"Encoder8",0],[6,"Option",313],[10,"UnivariateDistribution",0],[5,"Box",314],[5,"Sample",0],[10,"Clone",315],[5,"Samples",0],[5,"Vec",316],[1,"f64"],[1,"tuple"],[1,"usize"],[1,"unit"],[6,"Result",317],[5,"TypeId",318],[5,"Categorical",121],[5,"TruncatedCategorical",121],[10,"PartialEq",319],[5,"Formatter",320],[8,"Result",320],[10,"Debug",320],[10,"Ord",319],[10,"IntoIterator",321],[1,"i64"],[6,"Ordering",319],[10,"PartialOrd",319],[5,"Gaussian",182],[5,"TruncatedGaussian",182],[5,"WithReplacement",182],[5,"WithoutReplacement",182]],"r":[],"b":[],"c":"OjAAAAAAAAA=","e":"OzAAAAEAAOEAJAAAAAAACwANABoAAwAkAAYALAAAADMABwA8AAAAPgABAEEAAABDAAIARwABAEoAAQBOAA4AXgAlAIYAAwCNAAQAlAABAJgAAgCcAAAAoAAEAKYAEAC4AAAAuwAAAL4ABwDHAAkA0wAFANoAAwDiAAYA7QADAPMAAgD5AAAAAQEGAAoBAgAQAQEAFwELACQBFAA="}]\
]'));
if (typeof exports !== 'undefined') exports.searchIndex = searchIndex;
else if (window.initSearch) window.initSearch(searchIndex);
