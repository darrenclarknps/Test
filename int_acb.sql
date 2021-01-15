set trimspool on echo off scan off
spoo int_acb

CREATE OR REPLACE package int_acb is
-- *************************************************************************
-- acb interface database package procedures.
-- *************************************************************************
--my my my
-- * Ver    Date    By           Description
-- * 1.0  25/05/04  T.Goodley    Initial Creation.
-- * 1.1  ?         ?            Various changes specific to Islington.
-- * 1.2  10/03/07  T.Goodley    Fix bug with ContactDetails start tag.
-- *                             Fix bug in procedure write_output.
-- *                             Remove MobilityCategory.
-- *                             Add TRANS - Transgender sex code.
-- *                             Update ethnic lookup aray.
-- * 1.3  16/03/07  T.Goodley    Remove ale_app_refno <> 55808 from main cursor.
-- *                             Remove comments from ethnic lookup loop.
-- * 1.4  20/03/07  T.Goodley    EligibleNoBedrooms - added
-- *                             elsif l_result = 'L' then l_result = '0'.
-- * 1.5  04/04/07  T.Goodley    Fix bug in function PointsAllocated.
-- * 1.6  20/06/07  T.Goodley    Add TN rehousing list.
-- *                             Only add WM extra points if WM list is LIVE.
-- * 1.7  21/06/07  T.Goodley    Add TN to function IndividualReferenceCode.
-- * 1.8  26/06/07  T.Goodley    Remove leading space in IndividualReference
-- *                             and IndividualPin.
-- * 1.9  24/08/07  B.Marsh      Removed Vuln and temp from output
-- *                             and limited file.
-- * 2.0  30/11/07  T.Goodley    Add functionality for 'Deleted' cases.
-- * 2.1  28/04/08  P.Walker     Change CLQO to HCQQ (bm_int_acb6.sql)
-- * 2.2  06/11/08  T.Manning    MobilityCategory Added
-- * 2.3  22/02/10  V.Chin       Modified FUNCTION RegistrationStatus and
-- *                             FUNCTION rehousing_list
-- * 2.4  24/06/10  V.Chin       Changed MobilityCategory to use uppercase
-- *                             Added g_ver to show package version
-- * 2.5  16/09/10  V.Chin       Added function ContactDetails and XML lines
-- *                             for email and various telephone(s)
-- *                             Removed some logic in FUNCTION rehousing_list
-- * 2.6  22/09/10  V.Chin       Corrected Telephone TelUse="work"
-- * 2.7  16/10/17  T.Goodley    CR10191 - New logic for function Mobilitycategory.
-- *************************************************************************
g_ver CONSTANT VARCHAR2(50) := 'Ver2.7 16/10/2017';  -- Version tag.

-- declare global variables
camden       boolean       := false;
islington    boolean       := false;
an_error     boolean       := false;
yes          varchar2(3)   := 'yes';
no           varchar2(2)   := 'no';

type lang_rec_type is record                        -- Preferred language array.


(lang_iw  varchar2(10)
,lang_cbl varchar2(02));

type lang_type is table of lang_rec_type index by binary_integer;

lang_rec lang_type;

type ethnic_rec_type is record                      -- Ethnicity array.
(ethnic_iw  varchar2(10)
,ethnic_cbl varchar2(02));

type ethnic_type is table of ethnic_rec_type index by binary_integer;

ethnic_rec ethnic_type;

function find_aun                   -- Retrieve admin area code for given type.
(p_pro_refno   in number            -- Rent account Property reference.
,p_aun_type    in varchar2          -- Admin area code.
) return varchar2;

function rehousing_list         (p_app_refno          number)   return varchar2;



function IndividualReferenceCode(p_rli_code           varchar2) return varchar2;



function MainPartyDetails       (p_app_refno          number)   return varchar2;



function PointsAllocated        (p_app_refno          number
                                ,p_rli_code           varchar2) return varchar2;



function RegistrationStatus     (p_ale_app_refno      number
                                ,p_ale_rli_code       varchar2
                                ,p_ale_lst_code       varchar2) return varchar2;



function EligibleNoBedrooms     (p_app_refno          number
                                ,p_hty_code           varchar2) return varchar2;

function Mobilitycategory       (p_app_refno          number
                                ,p_apc_code           VARCHAR2) return varchar2;



function TemporaryAccommodation (p_app_refno          number)   return varchar2;



function UnString               (p_string             varchar2
                                ,p_field_no           number)   return varchar2;

function ContactDetails         (p_app_refno          number
                                ,p_type               varchar2) return varchar2;

procedure int542                (p_ihe_refno          number
                                ,p_changes_only       varchar2
                                ,p_cust_site          varchar2);

end int_acb;
/

show errors

CREATE OR REPLACE package body int_acb is

------------------------------------------------------------------------------
--not nice
--yoooooo
--nice
function find_aun                   -- Retrieve admin area code for given type.
(p_pro_refno   in number            -- Rent account Property reference.
,p_aun_type    in varchar2          -- Admin area code.
) return varchar2 is
cursor c01(p_pro_refno number) is
select --+ rule
       apr.apr_aun_code
,      aun.aun_auy_code
from   admin_units      aun
,      admin_properties apr
where  apr.apr_pro_refno = p_pro_refno
and    apr.apr_aun_code  = aun.aun_code;
p01    c01%rowtype;
cursor c02(p_pro_refno number) is
select apr_aun_code
from   admin_properties apr
where  apr_pro_refno = p_pro_refno;
p02    c02%rowtype;
cursor c03(p_aun_code_child varchar2
          ,p_type           varchar2) is
select --+ rule
       agr_aun_code_parent  aun_code
from   admin_groupings      agr
where  agr_aun_code_child  = p_aun_code_child
and    agr_auy_code_parent = p_type;
p03    c03%rowtype;
begin
open  c01(p_pro_refno);             -- Check is property directly linked to aun
fetch c01 into p01;                 -- type requested.
while c01%found loop
   if p01.aun_auy_code = p_aun_type
   then
      return p01.apr_aun_code;
   end if;
   fetch c01 into p01;
end loop;
close c01;
                                    -- Dir. link not found so check each branch.


open  c02(p_pro_refno);
fetch c02 into p02;
while c02%found loop
   open  c03(p02.apr_aun_code,p_aun_type);
   fetch c03 into p03;
   if    c03%found
   then
      return p03.aun_code;
   end if;
   close c03;
   fetch c02 into p02;
end loop;
close c02;
return null;
end find_aun;
------------------------------------------------------------------------------
------------------------------------------------------------------------------
FUNCTION rehousing_list (p_app_refno NUMBER) RETURN VARCHAR2 IS
CURSOR c1 IS                                        -- Camden Selection
SELECT DECODE(ale.ale_rli_code,'SHELTCBL',DECODE(ale.ale_lst_code,'LIVE',1,
                                          DECODE(ale.ale_als_active_ind,'Y',3)),
                               'REGISTER',DECODE(ale.ale_als_active_ind,'Y',2))
,      ale.ale_rli_code
FROM   applic_list_entries ale
WHERE  ale.ale_app_refno = p_app_refno
AND    ale.ale_rli_code||'' IN ('SHELTCBL','REGISTER')
ORDER BY 1;

p1     c1%ROWTYPE;

CURSOR c2 IS                                        -- Islington Selection.
SELECT ale.ale_rli_code                             -- LIVE list
,      ale.ale_lst_code
FROM   applic_list_entries ale
WHERE  ale.ale_app_refno = p_app_refno
AND    ale.ale_rli_code||'' IN ('HP','TF','HA','WL','OBW','TN')
AND    ale.ale_lst_code||'' = 'LIVE'
ORDER BY
       DECODE(ale.ale_rli_code,'HP',1,'TF',2,'HA',3,'WL',4,'OBW',5,'TN',6);

p2     c2%ROWTYPE;

CURSOR c3 IS                                        -- Islington Selection.
SELECT ale.ale_rli_code                             -- Suspended list
,      ale.ale_lst_code
FROM   applic_list_entries ale
WHERE  ale.ale_app_refno = p_app_refno
AND    ale.ale_rli_code||'' IN ('HP','TF','HA','WL','OBW','TN')
AND    ale.ale_lst_code||'' != 'LIVE'
ORDER BY
       DECODE(ale.ale_rli_code,'HP',1,'TF',2,'HA',3,'WL',4,'OBW',5,'TN',6);

p3     c3%ROWTYPE;

l_result         VARCHAR2(255);
l_tmp_points     INTEGER := 0;
l_highest_points INTEGER := -1;
l_HP_ind         BOOLEAN := FALSE;

BEGIN

   OPEN  c2;
   FETCH c2 INTO p2;
   IF c2%FOUND
   THEN
      CLOSE c2;

      FOR p2 IN c2 LOOP
         IF p2.ale_rli_code = 'HP'
         THEN
            l_result := p2.ale_rli_code;
            EXIT;
         END IF;

         l_tmp_points := PointsAllocated(p_app_refno,p2.ale_rli_code);

         IF l_tmp_points > l_highest_points
         THEN
            l_highest_points := l_tmp_points;
            l_result := p2.ale_rli_code;
         END IF;

      END LOOP;

   ELSE
      CLOSE c2;

      FOR p3 IN c3 LOOP
/*
      IF NOT (p3.ale_lst_code IN
                ('CANC','CLOA','HCQQ','CLUN','CNR','DCAN','HSED','REJ')
             OR (p3.ale_lst_code = 'SUSP' and p3.ale_rli_code = 'HP'))
      THEN
         GOTO next_list;
      END IF;
*/
      IF p3.ale_rli_code = 'HP'
      THEN
         l_result := p3.ale_rli_code;
         EXIT;
      END IF;

      l_tmp_points := PointsAllocated(p_app_refno,p3.ale_rli_code);

      IF l_tmp_points > l_highest_points
      THEN
         l_highest_points := l_tmp_points;
         l_result := p3.ale_rli_code;
      END IF;

      <<next_list>>
      NULL;

      END LOOP;

   END IF;

   RETURN l_result;

END rehousing_list;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
function IndividualReferenceCode(p_rli_code varchar2) return varchar2 is
l_result varchar2(255);
begin
      if    p_rli_code = 'HP'         then l_result := 'HML';
      elsif p_rli_code = 'WL'         then l_result := 'HGR';
      elsif p_rli_code = 'HA'         then l_result := 'NOM';
      elsif p_rli_code = 'TF'         then l_result := 'TRN';
      elsif p_rli_code = 'OBW'        then l_result := 'HGR';
      elsif p_rli_code = 'TN'         then l_result := 'TRN';
      end if;
return l_result;
end IndividualReferenceCode;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
function MainPartyDetails(p_app_refno number)   return varchar2 is
cursor c1 is
select replace(par.par_per_title,'|','')        Title
,      replace(par.par_per_forename,'|','')     Forename
,      replace(par.par_per_surname,'|','')      Surname
,      par.par_per_frv_fnl_code                 fnl_code
,      nvl(par.par_per_frv_feo_code,'NULL')     feo_code
,      decode(nvl(par.par_per_hou_hrv_hgo_code,'NULL'),'WHIT'   ,       '11'    ,
'WHIR'  ,       '12'    ,
'WHOT'  ,       '19'    ,
'BBCA'  ,       '21'    ,
'BBBA'  ,       '22'    ,
'BBOT'  ,       '29'    ,
'ABIN'  ,       '31'    ,
'ABPK'  ,       '32'    ,
'ABBA'  ,       '33'    ,
'CHIN'  ,       '34'    ,
'ABOA'  ,       '39'    ,
'MXWC'  ,       '41'    ,
'MXWB'  ,       '42'    ,
'MXWA'  ,       '43'    ,
'MXOT'  ,       '49'    ,
'FPNO'  ,       '52'    ,
'VIET'  ,       '53'    ,
'WHGR'  ,       '66'    ,
'WHTU'  ,       '67'    ,
'WHKU'  ,       '68'    ,
'OETG'  ,       '80'    ,
'NULL'  ,       '90') hgo_code
,      nvl(par.par_per_frv_fge_code,'U')        fge_code
,      par.par_per_date_of_birth                date_of_birth
,      par.par_refno                            par_refno
from   parties          par
,      involved_parties ipa
where  ipa.ipa_app_refno          = p_app_refno
and    sysdate between ipa.ipa_start_date and nvl(ipa.ipa_end_date,sysdate)
and    ipa.ipa_main_applicant_ind = 'Y'
and    par.par_refno              = ipa.ipa_par_refno;
p1         c1%rowtype;
cursor c2 is                                -- Select adr_refno for party.
select aus.aus_adr_refno adr_refno
from   address_usages aus
where  aus.aus_par_refno    = p1.par_refno
and    aus.aus_aut_fao_code = 'PAR'
and    aus.aus_aut_far_code = 'CONTACT'
and    trunc(sysdate)
       between trunc(aus.aus_start_date)
       and nvl(trunc(aus.aus_end_date),sysdate);
cursor c3 is                                -- Select adr_refno for application.


select aus.aus_adr_refno adr_refno
from   address_usages aus
where  aus.aus_app_refno    = p_app_refno
and    aus.aus_aut_fao_code = 'APP'
and    aus.aus_aut_far_code = 'APPLICATN'
and    trunc(sysdate)
       between trunc(aus.aus_start_date)
       and nvl(trunc(aus.aus_end_date),sysdate);
l_result   varchar2(255);
l_found    boolean := false;
l_lang_iw  varchar2(10);
l_lang_cbl varchar2(02);
l_sex      varchar2(13);
l_dob      varchar2(10);
l_ethnic_iw varchar2(10);
l_ethnic   varchar2(02) := '90';
l_adr_refno number(10);
begin
open  c1;
fetch c1 into p1;
l_found := c1%found;
close c1;
if l_found then

      l_lang_iw   := null;
      l_ethnic_iw := p1.hgo_code;
      open  c3;
      fetch c3 into l_adr_refno;
      close c3;
                                                  -- Sex determination.
   if    p1.fge_code in('U','TRANS')  then l_sex := 'unknown';
   elsif p1.fge_code in('MALE','M')   then l_sex := 'male';
   elsif p1.fge_code in('FEMALE','F') then l_sex := 'female';
   end if;
   l_dob := to_char(p1.date_of_birth,'YYYY-MM-DD');-- Date of birth.

   for x in ethnic_rec.first .. ethnic_rec.last loop
      if ethnic_rec(x).ethnic_iw = l_ethnic_iw then
         l_ethnic := ethnic_rec(x).ethnic_cbl;
         exit;
      end if;
   end loop;

   l_result := '|'||p1.Title   ||'|'||             -- Field 1.
                    p1.Forename||'|'||             -- Field 2.
                    p1.Surname ||'|'||             -- Field 3.
                    l_lang_cbl ||'|'||             -- Field 4.
                    l_sex      ||'|'||             -- Field 5.
                    l_dob      ||'|'||             -- Field 6.
                    l_ethnic   ||'|'||             -- Field 7.
                    l_adr_refno||'|';              -- Field 8.
end if;
return l_result;
end MainPartyDetails;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
function PointsAllocated(p_app_refno          number
                        ,p_rli_code           varchar2) return varchar2 is
cursor c1 is                                        -- Camden Selection.
select sum(arp.arp_base_points) base_points
from   applic_rule_points arp
where  arp.arp_app_refno    = p_app_refno
and    arp.arp_ale_rli_code = p_rli_code;
p1     c1%rowtype;
cursor c3 is                                        -- Islington actual points.
select sum(a1.arp_base_points)+nvl(wm.sum_whmb,0) actual_points
from applic_rule_points a1,
     (select sum(a2.arp_base_points) sum_whmb
      from   applic_list_entries     ale
      ,      applic_rule_points      a2
      where  a2.arp_brl_rca_code   = 'WHMB'
      and    a2.arp_ale_rli_code   = 'WM'
      and    a2.arp_app_refno      = p_app_refno
      and    ale.ale_app_refno     = a2.arp_app_refno
      and    ale.ale_rli_code      = a2.arp_ale_rli_code
      and    ale.ale_lst_code||''  = 'LIVE') wm
where a1.arp_app_refno = p_app_refno
and a1.arp_ale_rli_code = p_rli_code
and a1.arp_brl_rca_code <> 'WHMB'
group by nvl(wm.sum_whmb,0);
p3     c3%rowtype;
l_result         varchar2(255);
l_minimum_points integer :=0;
found_temp_aun   boolean := false;
begin

      open  c3;
      fetch c3 into p3;                             -- Get more list info.
      close c3;
      l_result := p3.actual_points;

return nvl(l_result,'0');
end PointsAllocated;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
FUNCTION RegistrationStatus     (p_ale_app_refno NUMBER
                                ,p_ale_rli_code  VARCHAR2
                                ,p_ale_lst_code  VARCHAR2)  RETURN VARCHAR2 IS
l_result          VARCHAR2(255);
l_pointsallocated NUMBER := pointsallocated(p_ale_app_refno,p_ale_rli_code);

BEGIN
    IF p_ale_lst_code = 'LIVE' AND l_pointsallocated >= 120
    THEN
       l_result := 'Registered';
    ELSIF p_ale_lst_code IN
          ('CANC','DCAN','CNR','CLOA','CLER','CLQO','CLUN','HSED','REJ')
    THEN
       l_result := 'Deleted';
    ELSE
       l_result := 'Suspended';
    END IF;

    RETURN l_result;

END RegistrationStatus;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
function EligibleNoBedrooms(p_app_refno          number
                           ,p_hty_code           varchar2)   return varchar2 is
cursor c1 is                                        -- Camden Selection.
select gan.gan_number_value
from   general_answers gan
where  gan.gan_que_refno = 1147
and    gan.gan_app_refno = p_app_refno;
p1     c1%rowtype;
l_result varchar2(255);
begin
if ISLINGTON then
      l_result := substr(p_hty_code,length(p_hty_code),1);
      if    l_result = 'N' then
            l_result := '7';
      elsif l_result = 'L' then
            l_result := '0';
      end if;
end if;
return l_result;
end EligibleNoBedrooms;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
function Mobilitycategory     (p_app_refno          number
                              ,p_apc_code           VARCHAR2) -- List Category
return varchar2 IS

CURSOR c481(cp_app_refno NUMBER) IS
SELECT SUBSTR(gan.gan_char_value,1,4) qval
FROM   general_answers    gan
WHERE  gan.gan_que_refno = 481
AND    gan.gan_app_refno = p_app_refno;

p481    c481%ROWTYPE;

l_result VARCHAR2(4):= NULL;

BEGIN

IF p_apc_code = 'RED'
THEN
   l_result := p_apc_code;
ELSE
   OPEN  c481(p_app_refno);
   FETCH c481 INTO p481;
   CLOSE c481;

   IF p481.qval IS NULL
   THEN
      l_result := 'G';
   ELSE
      l_result := p481.qval;
   END IF;
END IF;

RETURN l_result;


END Mobilitycategory;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
function TemporaryAccommodation(p_app_refno number)   return varchar2 is
cursor c1 is
select yes
from   list_statuses       lst
,      applic_list_entries ale
where  ale.ale_app_refno = p_app_refno
and    ale.ale_rli_code  = 'HOMELESS'
and    lst.lst_code      = ale.ale_lst_code
and    lst.lst_application_status_ind not in('X','H');
p1     c1%rowtype;
l_result varchar2(255);
begin
if    CAMDEN then
      open  c1;
      fetch c1 into p1;
      close c1;
      l_result := nvl(p1.yes,no);
end if;
return l_result;
end TemporaryAccommodation;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
function UnString(p_string   varchar2
                 ,p_field_no number)   return varchar2 is
l_result varchar2(255);
begin
l_result := substr(p_string,instr(p_string,'|',1,p_field_no)+1,
                   (instr(p_string,'|',1,p_field_no+1) -
                    instr(p_string,'|',1,p_field_no)-1));
return l_result;
end UnString;
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
function ContactDetails (p_app_refno  number
                        ,p_type       varchar2) return varchar2 is

cursor c1 (cp_app_refno number) is
select par.par_refno                            par_refno
from   parties          par
,      involved_parties ipa
where  ipa.ipa_app_refno          = cp_app_refno
and    sysdate between ipa.ipa_start_date and nvl(ipa.ipa_end_date,sysdate)
and    ipa.ipa_main_applicant_ind = 'Y'
and    par.par_refno              = ipa.ipa_par_refno;

p1     c1%rowtype;

cursor c2 (cp_par_refno number ) is
select cde_contact_value
,      cde_frv_cme_code
from   contact_details
where  cde_par_refno         = cp_par_refno
and    cde_frv_cme_code||''  = 'EMAIL'
and    cde_end_date is null
order by cde_start_date desc; -- latest first

p2     c2%rowtype;

cursor c3 (cp_par_refno number ) is
select cde_contact_value
,      cde_frv_cme_code
from   contact_details
where  cde_par_refno         = cp_par_refno
and    cde_frv_cme_code||'' in ('TENMOBI','HOME TEL','TEL','TELEPHONE')
and    substr(cde_contact_value,1,2) = '07'
and    cde_end_date is null
order by cde_start_date desc; -- latest first

p3     c3%rowtype;

cursor c4 (cp_par_refno number ) is
select cde_contact_value
,      cde_frv_cme_code
from   contact_details
where  cde_par_refno         = cp_par_refno
and    cde_frv_cme_code||'' in ('CONTACT','TENMOBI','HOME TEL','TEL'
                               ,'TELEPHONE')
and    substr(cde_contact_value,1,2) != '07'
and    cde_end_date is null
order by cde_start_date desc; -- latest first

p4     c4%rowtype;

l_return  varchar2(100);

begin

open  c1 (p_app_refno);
fetch c1 into p1;
close c1;

if p_type = 'EMAIL' then
   open  c2 (p1.par_refno);
   fetch c2 into p2;
   close c2;
   l_return := p2.cde_contact_value;
elsif p_type = 'MOBILE' then
   open  c3 (p1.par_refno);
   fetch c3 into p3;
   close c3;
   l_return := p3.cde_contact_value;
elsif p_type = 'WORKTEL' then
   open  c4 (p1.par_refno);
   fetch c4 into p4;
   close c4;
   if p4.cde_frv_cme_code in ('TEL','TELEPHONE')
   then
      l_return := p4.cde_contact_value;
   end if;
elsif p_type = 'HOMETEL' then
   open  c4 (p1.par_refno);
   fetch c4 into p4;
   close c4;
   if p4.cde_frv_cme_code in ('CONTACT','HOME TEL')
   then
      l_return := p4.cde_contact_value;
   end if;
end if;

return l_return;

end ContactDetails;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
/*                                                                       *
 *                   MAIN PROCEDURE - INT542                             *
 *                                                                       */
--------------------------------------------------------------------------
procedure int542(p_ihe_refno           number
                ,p_changes_only        varchar2
                ,p_cust_site           varchar2) is
l_RecordState   char(1);
l_ChangedBy     varchar2(4);
l_tab           integer :=-1;
qualifying_data boolean := false;
err          int_headers.ihe_error%type; -- Used for error handle.
l_status     char(1) := 'S';             -- Working status var.
i            integer := 0;               -- Record counter.
cursor main is
select ale.ale_app_refno                                app_refno
,      IndividualReferenceCode(ale.ale_rli_code)        IndividualReferenceCode
,      MainPartyDetails(ale.ale_app_refno)              MainPartyDetails
,      PointsAllocated
      (ale.ale_app_refno,ale.ale_rli_code)              PointsAllocated
,      RegistrationStatus
      (ale_app_refno,ale_rli_code,ale_lst_code)         RegistrationStatus
,      EligibleNoBedrooms
      (ale.ale_app_refno,ale.ale_defined_hty_code)      EligibleNoBedrooms
,      Mobilitycategory
      (ale.ale_app_refno,ale.ale_ala_hrv_apc_code)      Mobilitycategory
,      to_char(ale.ale_registered_date,'YYYY-MM-DD')    EffectiveDate
,      TemporaryAccommodation(ale.ale_app_refno)        TemporaryAccommodation
from   applic_list_entries ale
where  ale.ale_rli_code =
       int_acb.rehousing_list(ale.ale_app_refno)
minus
select acb.acb_app_refno
,      acb.acb_IndividualReferenceCode
,      acb.acb_MainPartyDetails
,      acb.acb_PointsAllocated
,      acb.acb_RegistrationStatus
,      acb.acb_EligibleNoBedrooms
,      acb.acb_Mobilitycategory
,      acb.acb_EffectiveDate
,      acb.acb_TemporaryAccommodation
from   int_acb_details acb
where  p_changes_only = 'Y'
and    acb.acb_latest = 'Y';
m1     main%rowtype;
cursor c1 is
select acb.acb_app_refno +
       decode(p_cust_site,'CAMDEN',0,800000)         IndividualReference
,      acb.acb_IndividualReferenceCode               IndividualReferenceCode
,      acb.acb_app_refno +
       decode(p_cust_site,'CAMDEN',0,800000)         IndividualPin
,      l_ChangedBy                                   DataSupplier
,      acb.acb_RecordState                           RecordState
,      UnString(acb.acb_MainPartyDetails,1)          govtalkCitizenNameTitle
,      UnString(acb.acb_MainPartyDetails,2)          govtalkCitizenNameForename
,      UnString(acb.acb_MainPartyDetails,3)          govtalkCitizenNameSurname
,      null                                          govtalkCitizenRegistration
,      UnString(acb.acb_MainPartyDetails,4)          govtalkPreferredLanguages
,      null                                          govtalkContactDetails
,      UnString(acb.acb_MainPartyDetails,5)          govtalkCitizenSex
,      UnString(acb.acb_MainPartyDetails,6)          govtalkBirthDate
,      'not verified'                                govtalkVerifiedBy
,      substr(nvl(adr.adr_line_1,'.'),1,35)          govtalkLine1
,      substr(nvl(adr.adr_line_2,'.'),1,35)          govtalkLine2
,      substr(adr.adr_line_3         ,1,35)          govtalkLine3
,      substr(adr.adr_line_4         ,1,35)          govtalkLine4
,      substr(adr.adr_postcode       ,1,35)          govtalkLine5
,      UnString(acb.acb_MainPartyDetails,7)          Ethnicity
,      acb.acb_Mobilitycategory                      Mobilitycategory
,      acb.acb_PointsAllocated                       PointsAllocated
,      acb.acb_RegistrationStatus                    RegistrationStatus
,      acb.acb_EligibleNoBedrooms                    EligibleNoBedrooms
,      acb.acb_EffectiveDate                         EffectiveDate
,      acb.acb_TemporaryAccommodation                TemporaryAccommodation
,      null                                          vulnerableperson
,      acb.acb_app_refno                             app_refno
from   addresses      adr
,      int_acb_details acb
where  acb.acb_ihe_refno    = p_ihe_refno
and    adr.adr_refno        = UnString(acb.acb_MainPartyDetails,8)
order by acb.acb_app_refno;
/*  Local Functions                                                     */
--------------------------------------------------------------------------
procedure Setup_Lookup_Arrays is
begin
                                         -- NO Preferred Language for Islington
                                         -- so just setup ethnicity array only.
      ethnic_rec(01).ethnic_iw := 'WHIT' ;ethnic_rec(01).ethnic_cbl := '11';
      ethnic_rec(02).ethnic_iw := 'WHIR' ;ethnic_rec(02).ethnic_cbl := '12';
      ethnic_rec(03).ethnic_iw := 'WHOT' ;ethnic_rec(03).ethnic_cbl := '19';
      ethnic_rec(04).ethnic_iw := 'BBCA' ;ethnic_rec(04).ethnic_cbl := '21';
      ethnic_rec(05).ethnic_iw := 'BBBA' ;ethnic_rec(05).ethnic_cbl := '22';
      ethnic_rec(06).ethnic_iw := 'BBOT' ;ethnic_rec(06).ethnic_cbl := '29';
      ethnic_rec(07).ethnic_iw := 'ABIN' ;ethnic_rec(07).ethnic_cbl := '31';
      ethnic_rec(08).ethnic_iw := 'ABPK' ;ethnic_rec(08).ethnic_cbl := '32';
      ethnic_rec(09).ethnic_iw := 'ABBA' ;ethnic_rec(09).ethnic_cbl := '33';
      ethnic_rec(10).ethnic_iw := 'CHIN' ;ethnic_rec(10).ethnic_cbl := '34';
      ethnic_rec(11).ethnic_iw := 'ABOA' ;ethnic_rec(11).ethnic_cbl := '39';
      ethnic_rec(12).ethnic_iw := 'MXWC' ;ethnic_rec(12).ethnic_cbl := '41';
      ethnic_rec(13).ethnic_iw := 'MXWB' ;ethnic_rec(13).ethnic_cbl := '42';
      ethnic_rec(14).ethnic_iw := 'MXWA' ;ethnic_rec(14).ethnic_cbl := '43';
      ethnic_rec(15).ethnic_iw := 'MXOT' ;ethnic_rec(15).ethnic_cbl := '49';
      ethnic_rec(16).ethnic_iw := 'FPNO' ;ethnic_rec(16).ethnic_cbl := '52';
      ethnic_rec(17).ethnic_iw := 'VIET' ;ethnic_rec(17).ethnic_cbl := '53';
      ethnic_rec(18).ethnic_iw := 'WHGR' ;ethnic_rec(18).ethnic_cbl := '66';
      ethnic_rec(19).ethnic_iw := 'WHTU' ;ethnic_rec(19).ethnic_cbl := '67';
      ethnic_rec(20).ethnic_iw := 'WHKU' ;ethnic_rec(20).ethnic_cbl := '68';
      ethnic_rec(21).ethnic_iw := 'OETG' ;ethnic_rec(21).ethnic_cbl := '80';
      ethnic_rec(22).ethnic_iw := 'NULL'; ethnic_rec(22).ethnic_cbl := '90';
      ethnic_rec(23).ethnic_iw := 'PNTS'; ethnic_rec(23).ethnic_cbl := '90';
      ethnic_rec(24).ethnic_iw := 'AFER' ;ethnic_rec(24).ethnic_cbl := '22';
      ethnic_rec(25).ethnic_iw := 'AFGH' ;ethnic_rec(25).ethnic_cbl := '22';
      ethnic_rec(26).ethnic_iw := 'AFNG' ;ethnic_rec(26).ethnic_cbl := '22';
      ethnic_rec(27).ethnic_iw := 'AFSM' ;ethnic_rec(27).ethnic_cbl := '22';
end Setup_Lookup_Arrays;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
procedure process_error(p_err_message  varchar2) is
begin
 insert into int_errors
(ier_ihe_refno, ier_key,      ier_err)
 values
(p_ihe_refno,   m1.app_refno, p_err_message);
 an_error := true;
end process_error;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
procedure validate_exists(p_tag_name   varchar2
                         ,p_value      varchar2) is
begin
if rtrim(p_value) is null then
   process_error(p_tag_name||' has not been found but is mandatory');
end if;
end validate_exists;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
procedure validate_length(p_tag_name   varchar2
                         ,p_value      varchar2
                         ,p_max_length integer) is

begin
if length(p_value) > p_max_length then
   process_error(p_tag_name||' length must not exceed '||p_max_length||
                 ' characters. ('||p_value||')');
end if;
end validate_length;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
procedure validate_number (p_tag_name   varchar2
                          ,p_value      varchar2) is
l_num number;
begin
if nvl(p_value,0) != nvl(trunc(p_value),0) then
   process_error(p_tag_name||' ('||p_value||') must be a whole number');
end if;
l_num := p_value;          -- Test that value is numeric.
exception when value_error then
process_error(p_value||' is not a numeric value for '||p_tag_name);
end validate_number;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
procedure validate_address is
l_adr_refno number(10);
l_postcode  varchar2(10);
cursor c1 is
select adr.adr_postcode
from   addresses adr
where  adr.adr_refno = l_adr_refno;
begin
l_adr_refno := UnString(m1.MainPartyDetails,8); -- adr_refno is field 8
                                                -- in MainPartyDetails.
open  c1;
fetch c1 into l_postcode;
if c1%notfound then
   process_error('Applicant Address details cannot be found');
-- elsif l_postcode is null then
--    process_error('PostCode is blank');
end if;
close c1;
end validate_address;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
procedure validate is
begin
-- Check that mandatory fields fetched from database have values.
validate_exists('govtalk:CitizenNameSurname',UnString(m1.MainPartyDetails,3));
validate_exists('govtalk:BirthDate',UnString(m1.MainPartyDetails,6));
validate_exists('EligibleNoBedrooms',m1.EligibleNoBedrooms);

-- Check that values do not exceed the maximum length allowed by CBL.

      validate_length('IndividualPin',m1.app_refno+800000,6);

validate_length('PointsAllocated',    m1.PointsAllocated,10);
validate_length('EligibleNoBedrooms', m1.EligibleNoBedrooms,10);
-- Check that values are numeric and are integer (whole) numbers.
validate_number('PointsAllocated',    m1.PointsAllocated);
validate_number('EligibleNoBedrooms', m1.EligibleNoBedrooms);
-- Check address exists.
validate_address;
end validate;
--------------------------------------------------------------------------
--------------------------------------------------------------------------
procedure write_output(p_tag     varchar2
                      ,p_value    varchar2 default 'START TAG'
                      ,p_optional boolean default false) is
l_line varchar2(500) := p_value;
begin
if p_value is null and p_optional then        -- If optional and has null value
   goto the_end;                              -- then do not output line.
end if;
if    p_value = 'START TAG' then
      l_line := '<'||p_tag||'>';
elsif p_value = 'END TAG'   then
      l_line := '</'||p_tag||'>';
      l_tab := l_tab -1;
elsif p_value is null       then
      l_line := '<'||p_tag||'/>';
else                                   -- Tag with value.
      l_line := replace(l_line,'&', '&amp;');
      l_line := replace(l_line,'<', '&lt;');
      l_line := replace(l_line,'>', '&gt;');
      l_line := replace(l_line,'''','&apos;');
      l_line := replace(l_line,'"', '&quot;');
      l_line := '<'||p_tag||'>'||l_line||'</'||p_tag||'>';
end if;
l_line := rpad(chr(9),l_tab,chr(9))||l_line; -- Add tabs to show hierarchical
                                             -- structure.
insert into int_acb_output values (i,l_line);
if    p_value = 'START TAG' then
      l_tab := l_tab +1;
end if;
i := i+1;
<<the_end>>
null;
end write_output;
--------------------------------------------------------------------------
begin                                  -- Main Block.
savepoint savepoint_1;                 -- Set point incase of ORA- failure.
dbms_output.put_line('INT542: Parameters:');
dbms_output.put_line('INT542: Changes Only.............'||p_changes_only);
dbms_output.put_line('INT542: Customer Site............'||p_cust_site);
dbms_output.put_line('INT542:');
dbms_output.put_line('INT542: int_acb..................'||int_acb.g_ver);
dbms_output.put_line('INT542:');

-- Set global variables to indicate the customer site.
 islington := true;
 l_ChangedBy := '00AU';
Setup_Lookup_Arrays;
loop
  delete from int_acb_output           -- Delete old temporary output data.
  where  rownum <=100;
  if sql%rowcount = 0 then
     exit;
  end if;
  commit;
end loop;
commit;
savepoint savepoint_1;                 -- Re-establish save point after commit.
open  main;
fetch main into m1;
while main%found loop                  -- Main loop to fetch data.
-- Initilise variables.
  l_RecordState := 'N';
  an_error      := false;
  validate;                            -- Check that data is valid.
  if an_error then
     goto read_next_record;
  end if;
  update int_acb_details
  set    acb_latest    = null
  where  acb_app_refno = m1.app_refno
  and    acb_latest is not null;
  if sql%found then                    -- If interfaced before then
     l_RecordState := 'M';             -- set RecordState to 'M'.
  end if;
  insert into int_acb_details
 (acb_ihe_refno
 ,acb_RecordState
 ,acb_app_refno
 ,acb_IndividualReferenceCode
 ,acb_MainPartyDetails
 ,acb_PointsAllocated
 ,acb_RegistrationStatus
 ,acb_EligibleNoBedrooms
 ,acb_Mobilitycategory
 ,acb_EffectiveDate
 ,acb_TemporaryAccommodation
 ,acb_latest)
  values
 (p_ihe_refno
 ,l_RecordState
 ,m1.app_refno
 ,m1.IndividualReferenceCode
 ,m1.MainPartyDetails
 ,m1.PointsAllocated
 ,m1.RegistrationStatus
 ,m1.EligibleNoBedrooms
 ,m1.Mobilitycategory
 ,m1.EffectiveDate
 ,m1.TemporaryAccommodation
 ,'Y');
  qualifying_data := true;
  <<read_next_record>>
  fetch main into m1;
end loop;                              -- End main property loop.
close main;
-- Now lets produce the output ready for int542o.sql
if not qualifying_data then            -- No data found to extract.
   goto no_data;
end if;
write_output('?xml version="1.0"?');
write_output('CBLMessage xmlns:xsi="http://www.w3.org/2000/10/XMLSchema-instance
" xmlns:govtalk="http://www.govtalk.gov.uk/people/AddressAndPersonalDetails" xsi:schemaLocation="http://www.homeconnections.gov.uk file:///C:\homeconnections\xsd\CBLMessageWrapper.xsd" xmlns="http://www.homeconnections.gov.uk"');

write_output('DataTransferDate',to_char(sysdate,'YYYY-MM-DD'));
write_output('ChangedBy',l_ChangedBy);
for p1 in c1 loop
  m1.app_refno := p1.app_refno;
  write_output('CBLIndividual');
  write_output('IndividualReference',to_char(p1.IndividualReference,'fm099999'));

  write_output('IndividualReferenceCode',p1.IndividualReferenceCode,true);
  write_output('IndividualPin',to_char(p1.IndividualPin,'fm099999'));
  write_output('DataSupplier',p1.DataSupplier);
  write_output('RecordState',p1.RecordState);
  write_output('Individual');
  write_output('IndividualDetails');
  write_output('govtalk:CitizenName');
    write_output('govtalk:CitizenNameTitle',p1.govtalkCitizenNameTitle,true);
    write_output('govtalk:CitizenNameForename',p1.govtalkCitizenNameForename,true);
    write_output('govtalk:CitizenNameSurname',p1.govtalkCitizenNameSurname);
  write_output('govtalk:CitizenName','END TAG');
  write_output('govtalk:CitizenRegistration',p1.govtalkCitizenRegistration);
  write_output('govtalk:PreferredLanguages',p1.govtalkPreferredLanguages,true);

  write_output('govtalk:ContactDetails');
  IF int_acb.ContactDetails(p1.app_refno,'EMAIL') IS NOT NULL
  THEN
    write_output('govtalk:Email');
    write_output('govtalk:EmailAddress'
                ,int_acb.ContactDetails(p1.app_refno,'EMAIL'));
    write_output('govtalk:Email','END TAG');
  END IF;
  IF int_acb.ContactDetails(p1.app_refno,'MOBILE') IS NOT NULL
  THEN
    write_output('govtalk:Telephone TelMobile="yes"');
    write_output('govtalk:TelNationalNumber'
                ,int_acb.ContactDetails(p1.app_refno,'MOBILE'));
    write_output('govtalk:Telephone','END TAG');
  END IF;
  IF int_acb.ContactDetails(p1.app_refno,'WORKTEL') IS NOT NULL
  THEN
    write_output('govtalk:Telephone TelUse="work" TelPreferred="yes"');
    write_output('govtalk:TelNationalNumber'
                ,int_acb.ContactDetails(p1.app_refno,'WORKTEL'));
    write_output('govtalk:Telephone','END TAG');
  END IF;
  IF int_acb.ContactDetails(p1.app_refno,'HOMETEL') IS NOT NULL
  THEN
    write_output('govtalk:Telephone TelUse="home" TelPreferred="no"');
    write_output('govtalk:TelNationalNumber'
                ,int_acb.ContactDetails(p1.app_refno,'HOMETEL'));
    write_output('govtalk:Telephone','END TAG');
  END IF;
  write_output('govtalk:ContactDetails','END TAG');

  write_output('govtalk:CitizenSex',p1.govtalkCitizenSex,true);
  write_output('govtalk:CitizenBirthDate');
    write_output('govtalk:BirthDate',p1.govtalkBirthDate);
    write_output('govtalk:VerifiedBy',p1.govtalkVerifiedBy);
  write_output('govtalk:CitizenBirthDate','END TAG');
  write_output('IndividualDetails','END TAG');
  write_output('IndividualAddress');
  write_output('ResidentialAddress');
  write_output('govtalk:A_5LineAddress');
  write_output('govtalk:Line',p1.govtalkLine1);
  write_output('govtalk:Line',p1.govtalkLine2);
  write_output('govtalk:Line',p1.govtalkLine3,true);
  write_output('govtalk:Line',p1.govtalkLine4,true);
  write_output('govtalk:Line',p1.govtalkLine5,true);
  write_output('govtalk:A_5LineAddress','END TAG');
  write_output('ResidentialAddress','END TAG');
  write_output('IndividualAddress','END TAG');
  write_output('Ethnicity',p1.Ethnicity);
  write_output('MobilityCategory',p1.Mobilitycategory);
  write_output('Individual','END TAG');
  write_output('IndividualAdminStructure');
  write_output('PointsAllocated',p1.PointsAllocated);
  write_output('RegistrationStatus',p1.RegistrationStatus);
  write_output('EligibleNoBedrooms',p1.EligibleNoBedrooms);
  write_output('EffectiveDate',p1.EffectiveDate);
  write_output('IndividualAdminStructure','END TAG');
  write_output('CBLIndividual','END TAG');
end loop;
write_output('CBLMessage','END TAG');
<<no_data>>
if i = 0 then
   l_status := 'Z';
else
   dbms_output.put_line('INT542: '||i||' Lines Processed.');
end if;
dbms_output.put_line('INT542: ');
update int_headers                     -- Flag the success to the headers table.


set    ihe_end    = sysdate
,      ihe_num    = i                  -- No. records to output.
,      ihe_status = l_status
,      ihe_param1 = p_changes_only
where  ihe_refno  = p_ihe_refno;
exception when others                  -- Flag error if exception occurs.
then
   rollback to savepoint_1;
   dbms_output.put_line('INT542: Oracle error has occurred. ');
   dbms_output.put_line('INT542: App ref: '||m1.app_refno);
   err  := substr(to_char(i)||' '||sqlerrm,  1,255);
   dbms_output.put_line(err);
   update int_headers
   set    ihe_end    = SYSDATE
   ,      ihe_status = 'F'
   ,      ihe_error  = err
   ,      ihe_param1 = p_changes_only
   where  ihe_refno  = p_ihe_refno;
   err  := substr(to_char(i)||' '||sqlerrm,255,255);
   dbms_output.put_line(err);
end int542;
--------------------------------------------------------------------------
end int_acb;
/
show errors
drop   public synonym int_acb;
create public synonym int_acb for int_acb;
GRANT  EXECUTE ON     int_acb TO fsc, hou_full, workflow;
spool off
