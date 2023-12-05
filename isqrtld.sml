fun length nil = 0 | length (x::xs) = 1 + (length xs); (*Function to calculate
  the length of a given list*)
fun reverse nil = nil | reverse (x::xs) = (reverse xs) @ [x];(*Function to
  reverse a given list*)
fun add_zero ( l1 , x) =
    if x <= 0 then l1
      else add_zero ([0]@l1,x-1);(*Function to add zeros to a given list*)
fun subtr ( v1:int list, v2:int list ,v3:int list , n: int, carry: int)
  =(*Function to subtract two given int lists*)
        if n=0 then v3
        else
          if hd(v1)-hd(v2)-carry < 0 then
            subtr(tl(v1),tl(v2),[10+hd(v1)-hd(v2)-carry]@v3,n-1,1)
          else
            subtr(tl(v1),tl(v2),[hd(v1)-hd(v2)-carry]@v3,n-1,0);
fun sub_list( v1 , v2 ) =(*Function to subtract two given int lists*)
  subtr(reverse(v1),reverse(add_zero( v2,length(v1)-length(v2))),[],length(v1),0);
fun addr (v1: int list, v2: int list, v3: int list, n:int, carry: int)
  =(*Function to add two given int lists*)
        if n=0 then v3
        else
          if hd(v1)+hd(v2)+carry > 9 then
            addr(tl(v1),tl(v2),[hd(v1)+hd(v2)+carry-10]@v3,n-1,1)
          else
            addr(tl(v1),tl(v2),[hd(v1)+hd(v2)+carry]@v3,n-1,0);
fun addlist (v1: int list, v2: int list) =(*Function to add two given int lists*)
  if length(v1) > length(v2) then
        addr(reverse(add_zero((v1),1)),reverse(add_zero((v2),length(v1)-length(v2)+1)),[],length(v1)+1,0)
  else
         addr(reverse(add_zero((v2),1)),reverse(add_zero((v1),length(v2)-length(v1)+1)),[],length(v2)+1,0);
fun multplr( v1: int list, v2: int list, x: int, n:int, carry: int) =(*Function to multiply a given int list with an integer*)
  if x=0 then [carry]@v2
  else
   multplr (tl(v1),[(hd(v1)*n+carry) mod 10]@v2,x-1,n,(hd(v1)*n+carry) div 10);

fun multiplier( v1: int list, n: int) =(*Function to multiply a given int list with an integer*)

  multplr(reverse(v1),[],length(v1),n,0);
fun ldngzrorem( v1 : int list) = (*Function to remove leading zeros from a given int list*)
  if length(v1) <= 1 then v1
  else if hd(v1)=0 then ldngzrorem(tl(v1))
  else v1;
fun gte (v1: int list, v2:int list)=(*Function to compare two given int lists*)
  if length(v1) > length(v2) then true
  else if length(v1) < length(v2) then false
  else if length(v1)=1 andalso hd(v1)>= hd(v2) then true
  else if length(v1)=1 andalso hd(v1) < hd(v2) then false
  else if hd(v1) > hd(v2) then true
  else if hd(v1) < hd(v2) then false
  else gte(tl(v1),tl(v2));
fun convert (xi: char list, n: int, y:int list)=(*Function to convert a given string to an int list*)
        if n=0 then reverse(y)
        else
        let val x = hd(xi)
        in
         convert(tl(xi),n-1,[ord x - ord #"0"] @ y)
        end;
fun converter(x : string)=convert(explode(x),length(explode(x)),[]);(*Function to convert a given string to an int list*)
fun converter_back(x: int list,y: int,z : char list)=(*Function to convert a given int list to a string*)
  if y=0 then implode(reverse(z))
  else
    let val xi = hd(x)
    in
      converter_back(tl(x),y-1,[chr (48+xi)]@z)
    end;
fun convert_back( x: int list)=converter_back(x,length(x),[]);(*Function to convert a given int list to a string*)
fun ge (v1: int list,v2:int list)=gte(ldngzrorem(v1),ldngzrorem(v2));(*Function to compare two given int lists*)
fun sqrt( v1: int list,left : int list,remd: int list ,otpt : int list ,n: int)=(* Function to finally execute long division*)
  if n=0 then (convert_back(reverse(otpt)),convert_back(ldngzrorem(remd)))
    else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[9],9))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[1,8]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[9],9)),[9]@otpt,n-1)
      else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[8],8))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[1,6]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[8],8)),[8]@otpt,n-1)
      else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[7],7))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[1,4]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[7],7)),[7]@otpt,n-1)
      else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[6],6))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[1,2]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[6],6)),[6]@otpt,n-1)
      else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[5],5))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[1,0]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[5],5)),[5]@otpt,n-1)
      else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[4],4))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[8]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[4],4)),[4]@otpt,n-1)
      else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[3],3))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[6]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[3],3)),[3]@otpt,n-1)
      else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[2],2))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[4]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[2],2)),[2]@otpt,n-1)
      else if ge(remd@[hd(v1),hd(tl(v1))],multiplier(left@[1],1))
  then
    sqrt(tl(tl(v1)),addlist(left@[0],[2]),sub_list(remd@[hd(v1),hd(tl(v1))],multiplier(left@[1],1)),[1]@otpt,n-1)
  else sqrt(tl(tl(v1)),left@[0],remd@[hd(v1),hd(tl(v1))],[0]@otpt,n-1);

 fun isqrtld( v1: string)= (*The function that calls our recursive long division
function*)
   if length(converter(v1)) mod 2 = 1 then
     sqrt([0]@converter(v1),[0],[0],[],length(converter(v1)) div 2 + 1)
   else sqrt(converter(v1),[0],[0],[],length(converter(v1)) div 2);
