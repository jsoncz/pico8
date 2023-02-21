pico-8 cartridge // http://www.pico-8.com
version 39
__lua__
gamesec = 0 --seconds
gamemin = 0 --minutes
framesec = 0 --frame count 0~29
fr256 = 0 --frame count 0~29
framesleft = 0 --for waiting
framesp = {} -- for two players
mode = 0 --title

score = {}
lines = {}
combo = {}
multi = {}

hiscore_a_n = {}
hiscore_a_v = {}
hiscore_b_n = {}
hiscore_b_v = {}

fadestate = 0 -- <0 = black, >0 = white

-- the zero character is translated as the "ed" glyph.

char2ascii = {
["0"]=48,["1"]=49,["2"]=50,["3"]=51,["4"]=52,["5"]=53,["6"]=54,["7"]=55,
["8"]=56,["9"]=57,[" "]=32,["!"]=33,["."]=46,["-"]=45,["a"]=65,["b"]=66,
["c"]=67,["d"]=68,["e"]=69,["f"]=70,["g"]=71,["h"]=72,["i"]=73,["j"]=74,
["k"]=75,["l"]=76,["m"]=77,["n"]=78,["o"]=79,["p"]=80,["q"]=81,["r"]=82,
["s"]=83,["t"]=84,["u"]=85,["v"]=86,["w"]=87,["x"]=88,["y"]=89,["z"]=90,
["\000"]=188}

ascii2char = {
[65]="a",[66]="b",[67]="c",[68]="d",[69]="e",[70]="f",[71]="g",[72]="h",
[73]="i",[74]="j",[75]="k",[76]="l",[77]="m",[78]="n",[79]="o",[80]="p",
[81]="q",[82]="r",[83]="s",[84]="t",[85]="u",[86]="v",[87]="w",[88]="x",
[89]="y",[90]="z",[48]="0",[49]="1",[50]="2",[51]="3",[52]="4",[53]="5",
[54]="6",[55]="7",[56]="8",[57]="9",[32]=" ",[33]="!",[46]=".",[45]="-",
[188]="\000"}

cdata_ok = false

graypal = {1,13,6,7}

function setrotate(f)
end

function setfade(f)
  for i=1,4 do
    local pi = i + f
    if (pi < 1) i = 1
    if (pi > 4) pi = 4
    pal(graypal[i],graypal[pi],1)
  end
end --function

function strget(addr, len)
  local r = ""

  for i=0,(len-1) do
    local b = peek(addr + i)
    if (b == 0) return r
    printh("r used to be " .. r)
   	r = r .. ascii2char[b]
	   printh("r is now " .. r)
  end
  
  return r
end -- function

function strput(addr,str,len)

  local l = #str
  if (l > len) l = len
  
  for i=0,(l-1) do
    local b = char2ascii[sub(str,i+1,i+1)]
	   poke(addr+i, b)
  end
  if (l < len) poke(addr+l, 0)
  
end

function load_hiscore()

  local t = 0
  if (cdata_ok) t = dget(9)

  if (t != 0) then
  
    for i=0,2 do
      hiscore_a_n[i+1] = strget(0x5e00 + (6*i), 6)
      hiscore_b_n[i+1] = strget(0x5e00 + 18 + (6*i), 6) -- 36 bytes, or 9 int values total
	
	     hiscore_a_v[i+1] = dget(9 + i)
	     hiscore_b_v[i+1] = dget(12 + i)
    end
  else -- load defaults
  
    hiscore_a_n = {"b.g.s", "alexey", "vadim"}
    hiscore_a_v = {100, 50, 25} -- fixme these scores are intentionally low so that they're easy to debug
  
    hiscore_b_n = {"zep", "jelpi", "pico-8"}
    hiscore_b_v = {5000, 2500, 1250}
  end
  
end

function save_hiscore()

  for i=0,2 do
    strput(0x5e00 + (6*i), hiscore_a_n[i+1], 6)
    strput(0x5e00 + 18 + (6*i), hiscore_b_n[i+1], 6) -- 36 bytes, or 9 int values total
	
    dset (9+i , hiscore_a_v[i+1])
    dset (12+i , hiscore_b_v[i+1])
  end
  
end

function add_hiscore(mode, name, score)

  if (mode == 0) then -- a mode
  
  for i=1,3 do
    if (score > hiscore_a_v[i]) then
      for j=3,i+1,-1 do
        hiscore_a_n[j] = hiscore_a_n[j-1] 
        hiscore_a_v[j] = hiscore_a_v[j-1] 
      end
      hiscore_a_n[i] = name
      hiscore_a_v[i] = score
      save_hiscore()
      return
    end
  end

  else -- b mode
    for i=1,3 do
      if (score > hiscore_b_v[i]) then
	for j=3,i+1,-1 do
	  hiscore_b_n[j] = hiscore_b_n[j-1] 
	  hiscore_b_v[j] = hiscore_b_v[j-1] 
	end
	hiscore_b_n[i] = name
	hiscore_b_v[i] = score
	save_hiscore()
	return
      end
    end
  end -- if

end -- function

function _init()
	 music(1)
  cdata_ok = cartdata("bgs_tetromix")
  framesec = 0
  fr256 = 0
  mode = 0
  cheatcode = 0
  framesleft = 0
  menu_hrt = false
  menu_bgm = 0
  menu_spd = 0
  menu_lvl = 0
  clip()
  pal()
  color()
  camera ()
  palt()
  palt(0, false)
  palt(14, true)
  load_hiscore()
  for i=0,1 do framesp[i]=0 end
  for i=0,1 do p_mode[i]=0 end
  for i=0,1 do cleared_y[i]={} end
end

title_drawn = 0
oldinp = 0

buttonsheld={}

function btnr_update()
local res = btn()
for i=0,15 do
  if (band(res,shl(1,i)) != 0) then
    buttonsheld[i] = buttonsheld[i]+1
  else
    buttonsheld[i] = 0
  end
end

end -- function

function btnr(i,p)

local res = 0
for bi=0,15 do
  if ( (buttonsheld[bi] == 1) or ((buttonsheld[bi] >= 8) and ((buttonsheld[bi] % 2) == 0)) ) then
    res = bor(res, shl(1,bi))
  end
end

if ((i) and (p)) then
  return (band(shr(res,p*8+i),1) > 0)
elseif (i) then
  return (band(shr(res,i),1) > 0)
else
  return res
end -- if i and p

end -- function

function btn1(i,p)

local res = 0
for bi=0,15 do
  if (buttonsheld[bi] == 1) then
    res = bor(res, shl(1,bi))
  end
end

if ((i) and (p)) then
  return (band(shr(res,p*8+i),1) > 0)
elseif (i) then
  return (band(shr(res,i),1) > 0)
else
  return res
end -- if i and p

end

function reset_game()

mode = 0
menus_first = 0
menu_sel = 0
over_mode = 0
p_mode[0] = 0
p_mode[1] = 0

for iy=1,15 do
 for ix=0,4 do
  mset(2+ix,iy,mget(31,iy))
  mset(17+ix,iy,mget(31,iy))
 end
end

end

function draw_title()

map(32,0,0,0,16,16)
if ((fr256 % 64) == 2) then
  pal(4,10,0)
  pal(9,7,0)
  pal(10,7,0)
elseif ((fr256 % 64) < 3) then
  pal(4,9,0)
  pal(9,10,0)
  pal(10,7,0)
end
spr(78,104,40,2,2,false,false)
if ((fr256 % 64) < 3) then
  pal(4,4,0)
  pal(9,9,0)
  pal(10,10,0)
end
prints("original game", 8, 80,0,7)
prints("@ 2007-09 nbgi", 8, 88,0,7)
prints("reprogrammed", 8, 96,0,7)
prints("@ 2016 pico-8", 8, 104,0,7)
prints("  community", 8, 112,0,7)

end

function irnd(x)
return flr(rnd(x))
end

bricks = {0,27,28,29,30}

-- triomino positions
--        '''        :.
trx = {-1, 0, 1,  0, 1, 0}
try = { 0, 0, 0,  0, 0, 1}

ct_r = {0,0} -- rotation of triomino
ct_x = {-1,-1} -- x position of triomino
ct_y = {-1,-1} -- y position of triomino

cttype = {-1, -1} -- triomino type (straight or angled)
ctcolor = {-1,-1,-1, -1,-1,-1} --triomino colors
ctpos_x = { 0, 0, 0,  0, 0, 0} --triomino rel x
ctpos_y = { 0, 0, 0,  0, 0, 0} --triomino rel y

nttype = {-1, -1} -- next trm type (straight or angled)
ntcolor = {-1,-1,-1, -1,-1,-1} --next trm colors
ntpos_x = { 0, 0, 0,  0, 0, 0} --next trm rel x
ntpos_y = { 0, 0, 0,  0, 0, 0} --next trm rel y

hdtype = {-1, -1} -- held trm type (straight or angled)
hdcolor = {-1,-1,-1, -1,-1,-1} --held trm colors
hdpos_x = { 0, 0, 0,  0, 0, 0} --held trm rel x
hdpos_y = { 0, 0, 0,  0, 0, 0} --held trm rel y

garbage={} -- how many garbage lines left

holdlock = {}

function holdpiece(p)

  if (holdlock[p] == true) return
  holdlock[p] = true
  cttype[p],hdtype[p] = hdtype[p],cttype[p]
  for i=1,3 do
    ctcolor[p*3+i],hdcolor[p*3+i] = hdcolor[p*3+i],ctcolor[p*3+i]
    ctpos_x[p*3+i],hdpos_x[p*3+i] = hdpos_x[p*3+i],ctpos_x[p*3+i]
    ctpos_y[p*3+i],hdpos_y[p*3+i] = hdpos_y[p*3+i],ctpos_y[p*3+i]
  end
  
  if (cttype[p] == -1) new_block(p)
  if (test_solid(0,0,p)) then
    cttype[p],hdtype[p] = hdtype[p],cttype[p]
  for i=1,3 do
    ctcolor[p*3+i],hdcolor[p*3+i] = hdcolor[p*3+i],ctcolor[p*3+i]
    ctpos_x[p*3+i],hdpos_x[p*3+i] = hdpos_x[p*3+i],ctpos_x[p*3+i]
    ctpos_y[p*3+i],hdpos_y[p*3+i] = hdpos_y[p*3+i],ctpos_y[p*3+i]
  end
  end
end

function spawnblock(p)
  ct_x[p] = 2
  ct_y[p] = 10

  for i=1,3 do
    ctpos_x[p*3+i] = trx[cttype[p]*3+i]
    ctpos_y[p*3+i] = try[cttype[p]*3+i]
  end
end --function

function move_block(dx,dy,p)
  ct_x[p] += dx
  ct_y[p] += dy
end

function move_if_solid(dx,dy,p)
  if (not test_solid(dx,dy,p)) move_block(dx,dy,p)
end

nt_x = {0,0,0}
nt_y = {0,0,0}

function test_rotated(dx,dy,p)

  for i=1,3 do
    if ((ct_x[p] + nt_x[3*p+i] + dx) < 0) return 1
    if ((ct_x[p] + nt_x[3*p+i] + dx) > 4) return 1
    if ((ct_y[p] + nt_y[3*p+i] + dy) < 0) return 1

    if (_tile_solid(ct_x[p] + nt_x[3*p+i] + dx, ct_y[p] + nt_y[3*p+i] + dy,p)) return true
  end

  return false
  
end --function

function rotate(r,p)

  if (cttype[p] == 3) return 0 --squares can't be rotated.

  if (r == 1) then
    for i=1,4 do
    nt_y[3*p+i] = -ctpos_x[3*p+i]
    nt_x[3*p+i] = ctpos_y[3*p+i]
    end
  elseif (r == -1) then
    for i=1,4 do
    nt_y[3*p+i] = ctpos_x[3*p+i]
    nt_x[3*p+i] = -ctpos_y[3*p+i]
    end
  end

  if (test_rotated(0,0,p)) then
    --doesn't fit, let's push a bit.
    if (not test_rotated(1,0,p)) then
      ct_x[p] += 1
    elseif (not test_rotated(-1,0,p)) then
      ct_x[p] -= 1
    else 
      sfx(0)
      return 0 --sorry, no rotations.
    end
  end

  for i=1,3 do
    ctpos_x[3*p+i] = nt_x[3*p+i]
    ctpos_y[3*p+i] = nt_y[3*p+i]
  end

  sfx(0)

end --function

function _tile_solid(x,y,p)

  local t = mget(2 + (15 * p) + x,13-y)
  return fget(t,0) --flag 0

end

function test_solid(dx,dy,p)

  for i=1,3 do
    if ((ct_x[p] + ctpos_x[3*p+i] + dx) < 0) return true
    if ((ct_x[p] + ctpos_x[3*p+i] + dx) > 4) return true
    if ((ct_y[p] + ctpos_y[3*p+i] + dy) < 0) return true

    if (_tile_solid(ct_x[p] + ctpos_x[3*p+i] + dx, ct_y[p] + ctpos_y[3*p+i] + dy,p)) return true
  end

  return false
end

function hard_drop(p)

  while (not test_solid(0,-1,p)) do
    ct_y[p] -=1
  end 
  framesp[p] = 0

end

cleared_y = {}
linescores = {2,5,15,60}

function add_garbage(p)

-- todo drop garbage instead

  garbage[p] = 0

end -- function

function drop_multi(p)

for ix=0,4 do
  mset(2 + (15*p) + ix,3,30)
  framesp[p] = 2
end
end

function actually_clear(p)

  for y1 in all(cleared_y[p]) do
  
    for y2=y1,19 do
      for x=0,9 do
        mset(2 + (15*p) + x,13-y2,mget(2 + (15*p) + x,13-(y2+1)))
      end
    end
    for x=0,9 do
      mset(2 + (15*p) + x,0,12) --clear top row.
    end
    del(cleared_y[p],y1)
  end
  
  if (game_mod == 0) and (game_spd[p] < 20) and (flr(lines[p] / 10) > game_spd[p]) then --advance a level
    game_spd[p] = flr(lines[p] / 10)
    --sfx(16)
  end
  
  if ((game_mod == 1) and (lines[p] <= 0)) then -- victory!
	you_win(p)
	if (game_2p) game_over(1-p)
  end

end --function

function gravity(p)

local r = false

 for ix=0,4 do
  for iy=0,12 do --enough for pretty much all situations
   if (not _tile_solid(ix,iy,p) and _tile_solid(ix,iy+1,p)) then
    
	local t = mget(2 + 15*p + ix, 13 - (iy+1))
	mset(2 + (15 * p) + ix, 13 - iy,t)
	local e = mget(31, 13 - (iy+1)) -- "clean" map area.
	mset(2 + (15 * p) + ix, 13 - (iy+1),e)
	r = true
   end --if
  end -- for iy
 end -- for ix
return r
end

function tcol(x,y,p)

local l = fget(mget(2+(15*p)+x,13-y))

return flr(shr(l,1)) -- remove the "solid" flag

end

clear_xy = {{},{}}
clear_xys = {{},{}} -- for sparks.

function xyp_existss(xy,p)
for i=1,count(clear_xys[p+1]) do
  if (clear_xys[p+1][i] == xy) return true 
end
return false
end

function add_clears(x,y,p)
  local xy = x + (5 * y)
  if (not xyp_existss(xy,p)) add(clear_xys[p+1],xy)
end

function xyp_exists(xy,p)
for i=1,count(clear_xy[p+1]) do
  if (clear_xy[p+1][i] == xy) return true 
end
return false
end

function add_clear(x,y,p)
  local xy = x + (5 * y)
  if (not xyp_exists(xy,p)) add(clear_xy[p+1],xy)
end

function check_line(p)
for ix=0,4 do 
  for iy=0,12 do -- will be enough
  
	local l = 1
	local f = tcol(ix,iy,p)
	 
	if (ix < 3) then --no need to cycle through last two columns
	  -- horizontal lines 
	  for il = 1,(4-ix) do
	    f = band(f,tcol(ix+il,iy,p))
	    if (f > 0) l = (il+1)
	  end
	 
      if (l >= 3) then
	    for il=0,(l-1) do add_clear(ix+il,iy,p) end
	    if (framesp[p] < 6) framesp[p] = 6
	    if (l == 5) then --line spark
	      if (framesp[p] < 18) framesp[p] = 18
	      for y=0,12 do
		    for x=0,4 do
	          if (band(f,tcol(x,y,p)) > 0) add_clears(x,y,p) -- clear any blocks that have the same color
		    end
	      end
	      score[p] += 50 --spark bonus
	    end -- l==5
	  end -- l>=3
	end -- ix<3

	-- vertical lines
	l = 1
	f = tcol(ix,iy,p)
	
	for il = 1,(12-iy) do
	  f = band(f,tcol(ix,iy+il,p))
	  if (f > 0) l = (il+1)
	end
	
	if (l >= 3) then
	  for il=0,(l-1) do add_clear(ix,iy+il,p) end
	  if (framesp[p] < 6) framesp[p] = 6
	end --l >= 3

    end -- for iy
  end -- for ix

  local r = count(clear_xy[p+1])
  if ((r >= 5) and (framesp[p] < 12)) framesp[p] = 12

end -- function

function drop_block(p)

  if (test_solid(0,-1,p)) then
    --area occupied, put block into map
  
    for i=1,3 do
      mset(2 + (15*p) + ct_x[p] + ctpos_x[3*p+i],13-ct_y[p] - ctpos_y[3*p+i],27+ctcolor[p*3+i])
    end
    cttype[p] = -1
    --sfx(17)
    framesp[p] = flr(getdelay(p) / 4)
    if (framesp[p] < 4) framesp[p] = 4
  else
    --area free, drop block one line.
    ct_y[p] -= 1
    framesp[p] = getdelay(p)
  end

end --function

function draw_pf_next()

  local mp = 0
  if (game_2p) mp=1

  for ip=0,mp do
    for i=1,3 do
      sspr(ntcolor[ip*3+i]*4,16,4,4, (56*ip) + 46 + 4*(ntpos_x[ip*3+i]), 12+ 4*(ntpos_y[ip*3+i]))
    end
	   if (hdtype[ip] >= 0) then
      for i=1,3 do
        sspr(hdcolor[ip*3+i]*4,16,4,4, (56*ip) + 22 + 4*(hdpos_x[ip*3+i]), 12+ 4*(hdpos_y[ip*3+i]))
      end
    end
  end
end

function draw_pf_score()

if (not game_2p) then

if (game_mod == 0) then
  prints("score",64,64,7,0)
  printx(padstr(""..score[0], 5, "0"),112,72,true)
else
  prints("level",64,80,7,0)
  printx(padstr(""..lines[0], 2, "0"),112,72,true)
end
  prints("time",64,96,7,0)
printx(gamemin .. ":" .. padstr("" .. gamesec,2,"0"),112,104,true)

end --if game_2p

end --function


function draw_p_gameover(p)

  local xspr=43
  if (game_2p) xspr = 44
  
  for iy = 0, 9 - flr(framesp[p] / 2) do
    for ix = 0,4 do
      spr(xspr,16 + (p*56)+(ix*8),32+iy*8)
    end
  end

  if (framesp[p] == 0) then    
  	prints("game",(56*p+20),(7*8))
	  prints("over",(56*p+20),(8*8))
  end
end -- function

function draw_p_victory(p)

  for iy = 0, 9 - flr(framesp[p] / 2) do
    for ix = 0,4 do
      spr(45,16 + (p*56)+(ix*8),32+iy*8)
    end
  end

  if (framesp[p] == 0) then    
  	prints("you",(56*p+24),(7*8))
	  prints("win!",(56*p+20),(8*8))
  end
end -- function

function draw_game()
map(0,0,0,0,8,16) -- draw 1p playfield

if (game_2p) then
  map(16,0,64,0,8,16) -- draw 2p playfield
else
  map(8,0,64,0,8,16) -- draw stats window
end

for ip=0,1 do

  if (p_mode[ip] == 0) then -- press button to play
  
  elseif (p_mode[ip] == 1) then -- playing
  
    if (cttype[ip] >= 0) then
      for i=1,3 do
        spr(ctcolor[3*ip+i]+27, 16 + (64*ip) + (ct_x[ip] + ctpos_x[3*ip+i])*8, 32+(9 - ct_y[ip] - ctpos_y[3*ip+i])*8)
      end
    end

    local t = 0
    

  
  elseif (p_mode[ip] == 2) then -- game over
  
    draw_p_gameover(ip)
     
  elseif (p_mode[ip] == 3) then -- victory

    draw_p_victory(ip)
  
  end

end --for ip

draw_pf_next()
draw_pf_score()


end --function

delays = {30,25,22,20,18,16,14,12,10,9,8,7,6,5,5,4,4,3,3,3,2}

menu_sel = 0 --selected menu
game_mod = 0 -- game mode
game_stg = 0 --stage
game_spd = {} --speed
game_bgm = 1 --bgm selection
game_2p = false -- two player mode?
p_mode = {} -- player mode (empty / playing / game over / victory)

bgmptns = {1,-1,-1,-1}

function getdelay(p)
  local l = game_spd[p]+1
  if (l > count(delays)) l=count(delays)
  return delays[l]
end

fonttiles = {
["0"]=129,["1"]=130,["2"]=131,["3"]=132,["4"]=133,
["5"]=134,["6"]=135,["7"]=136,["8"]=137,["9"]=138,
["@"]=139,[" "]=128,["!"]=142,["."]=141,["-"]=140,
[":"]=143,["a"]=145,["b"]=146,["c"]=147,["d"]=148,
["e"]=149,["f"]=150,["g"]=151,["h"]=152,["i"]=153,
["j"]=154,["k"]=155,["l"]=156,["m"]=157,["n"]=158,
["o"]=159,["p"]=160,["q"]=161,["r"]=162,["s"]=163,
["t"]=164,["u"]=165,["v"]=166,["w"]=167,["x"]=168,["y"]=169,["z"]=170,["\000"]=171}

fnt2tiles = {
["0"]=177,["1"]=178,["2"]=179,["3"]=180,["4"]=181,
["5"]=182,["6"]=183,["7"]=184,["8"]=185,["9"]=186,
[":"]=187,[" "]=128,["!"]=219,["."]=218,["-"]=140,
[":"]=143,["a"]=224,["b"]=225,["c"]=226,["d"]=227,
["e"]=228,["f"]=229,["g"]=230,["h"]=231,["i"]=232,
["j"]=233,["k"]=234,["l"]=235,["m"]=236,["n"]=237,
["o"]=238,["p"]=239,["q"]=240,["r"]=241,["s"]=242,
["t"]=243,["u"]=244,["v"]=245,["w"]=246,["x"]=247,["y"]=248,["z"]=249}

function padstr(str,len,pad)
  if (#str >= len) return str
  return padstr(pad .. str,len,pad)
end

function printm(str,x,y) --print a line into map

local mx = x
local my = y

for i=1,#str do
  local ch = sub(str,i,i)
  mset(mx,my,fonttiles[ch])
  mx += 1
end
end --function

function printx(str,x,y,right)

local cx = x
if ((right) and (right == true)) cx -= (8 * #str)
local cy = y

local i=1

while (i <= #str) do

  local ch = sub(str,i,i)
  local sp = fnt2tiles[ch]
 
  if (ch == '^') then -- uppercase?
    ch = sub(str,i+1,i+1)
    sp = fnt2tiles[ch]-32
    spr(sp,cx,cy)
    cx += 8
    i += 1
  elseif (ch == '~') then
    cx = x
    cy += 8
  else
    spr(sp,cx,cy)
    cx += 8
  end
  
  i += 1
  
end --while
end --function

function prints(str,x,y,c,bc) --print a line with sprites

if (c) pal(7,c,0)
if (bc) pal(1,bc,0)

local cx = x
local cy = y

for i=1,#str do
  local ch = sub(str,i,i)
  spr(fonttiles[ch],cx,cy)
  cx += 8
end

if (c) pal(7,7,0)
if (bc) pal(1,1,0)

end --function

function printsr(str,x,y,c,bc) --print a right-aligned line with sprites

if (c) pal(7,c,0)
if (bc) pal(1,bc,0)

local cx = x - (8 * #str)
local cy = y

for i=1,#str do
  local ch = sub(str,i,i)
  spr(fonttiles[ch],cx,cy)
  cx += 8
end

if (c) pal(7,7,0)
if (bc) pal(1,1,0)

end --function
function printc(str,y,col) --print a line at the center of screen.
  shift = #str * 2
  print(str,64 - shift, y, col)
end

function printr(str,x,y,col) --print a right-aligned line
  shift = #str * 4
  print(str,x - shift, y, col)
end

function play_bgm(id)
  if (id < 1) then
    music(-1)
  else
    music(bgmptns[id])
  end
end

function blink(n)
  if (band(n,4) == 4) return true
  return false
end --function

menu_desc = {
"^go slow and easy~"..	
 "  in the 1^p mode~"..
 "as you clear~"..
 "      triominos!",
"^play ^triotos~"..
 "    with someone~"..
 "close to you.",
"^choose whether~"..
 " or not the game~"..
 "should play ^b^g^m."
}

function draw_menus()

map(48,0,0,0,16,16)

prints("mode  select", 16, 16,0,7)

prints("endless",  32, 32,0,7)
prints("vs.2p",    32, 40,0,7)
if (game_bgm > 0) then
  prints("music on", 32, 56,0,7)
else
  prints("music off", 32, 56,0,7)
end

local xsh = (fr256 % 32) / 2 - 4
if (xsh < 0) xsh = -xsh
if (xsh > 4) xsh = 4

local ypos = 32 + (8 * menu_sel)
if (menu_sel > 1) ypos += 8

spr(172, 16+xsh, ypos)

if (menu_desc[menu_sel+1]) then
 printx(menu_desc[menu_sel+1],0,88)
end

end --function

hisc_curchr = 0
hisc_chrval = 0

hisc_curname = ""

hisc_chars = {
"a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p",
"q","r","s","t","u","v","w","x","y","z",".","-","0","1","2","3",
"4","5","6","7","8","9","!","\000"}

function start_hiscore()
hisc_curchr = 0
hisc_curval = 0
hisc_curname = ""
mode = 15
end

function update_hiscore()

  if (hisc_curchr < 6) then
    if (btnp(2)) then
      hisc_curval += 1
  	  if (hisc_curval > (count(hisc_chars) - 1)) hisc_curval = 0 --skipping through the zero
    elseif (btnp(3)) then
      hisc_curval -= 1
	  if (hisc_curval < 0) hisc_curval = (count(hisc_chars) - 1) --skipping through the zero
    end
  end

  if (btn1(5) or btn1(0)) then
    hisc_curchr -= 1
	if (hisc_curchr < 0) hisc_curchr = 0
	for k,v in pairs(hisc_chars) do
	  if (sub(hisc_curname,hisc_curchr+1,hisc_curchr+1) == hisc_chars[k]) hisc_curval = (k-1)
	end
  elseif (btn1(4) or btn1(1)) then
    if (hisc_curval == (count(hisc_chars)-1)) then -- end character chosen
	  hisc_curname = sub(hisc_curname,1,(#hisc_curname)-1) -- remove the end character
	  add_hiscore(game_mod,hisc_curname,score[0])
	  reset_game()
	else
      hisc_curchr += 1
	  if (hisc_curchr > 5) then 
	    hisc_curchr = 6
	    hisc_curval = (count(hisc_chars) - 1)
	  end
	end  
  end
  
  if (hisc_curchr > 0) then
    hisc_curname = sub(hisc_curname,1,hisc_curchr) .. hisc_chars[hisc_curval+1]
  else
    hisc_curname = "" .. hisc_chars[hisc_curval+1]
  end
  
  if (btn1(4) or btn1(5)) then
  
  end
  
end

function draw_hiscore()
map(64,0,0,0,16,16)

  prints(hisc_curname,8,96)
  prints(padstr("" .. score[0],5,"0"), 80,96)
  if (blink(fr256)) spr(13,8 + (hisc_curchr * 8),96)
  
end

function _draw()
framesec+=1
fr256+=1
if (framesec >= 30) framesec -= 30 gamesec += 1
if (gamesec >= 60) gamesec -= 60 gamemin += 1
if (gamemin > 99) gamemin = 99 gamesec = 59
if (fr256 >= 256) fr256 -= 256
if ( mode == 0) return draw_title()
if ( mode == 1) return draw_menus()
if ( mode == 2) return draw_game()
if ( mode == 3) return draw_win()
if ( mode == 15) return draw_hiscore()

end

function update_title()
  if (btn1(4)) mode = 1 -- menus
end

menus_first = 0

function game_over(p)

  p_mode[p] = 2 -- game over mode
  framesp[p] = 20
  --sfx(11)
  music(-1)
end --function

function you_win(p)
  framesp[p] = 15
  p_mode[p] = 3 --you win screen
  music(-1)
  --sfx(11)
end

function gen_block(p)

  nttype[p] = irnd(2)
  -- make it so that blocks can't have all three colors
  local ncolors = {0,1}
  local r = irnd(3)
  if (r == 1) ncolors[2] = 2
  if (r == 2) ncolors[1] = 2
  
  for i=1,3 do
    ntpos_x[3*p+i] = trx[nttype[p]*3+i] 
    ntpos_y[3*p+i] = try[nttype[p]*3+i] 
    ntcolor[3*p+i] = ncolors[1+irnd(2)]
  end
end --function

function new_block(p)

  combo[p] = 1
  holdlock[p] = false
  cttype[p] = nttype[p]
  
  for i=1,3 do
  ctcolor[3*p+i] = ntcolor[3*p+i]
  end
  spawnblock(p)
  if (test_solid(0,0,p)) then
    game_over(p)
    if (game_2p) you_win(1-p)
    return false
  end

  gen_block(p) -- generate new block
  
  if (multi[p] > 0) then
    for i=1,( multi[p]) do ntcolor[3*p+i] = 3 end
	
	for i=3,2,-1 do
	  local j = 1 + irnd(i-1)
	  ntcolor[3*p+i],ntcolor[3*p+j]=ntcolor[3*p+j],ntcolor[3*p+i]
	end
	
	multi[p] = 0
  end
  
  return true
  
end

function start_game()

mode = 2
framesec = 0
gamesec = 0
gamemin = 0

  for i=0,1 do score[i] = 0 end
  for i=0,1 do combo[i] = 1 end
  for i=0,1 do multi[i] = 0 end
  for i=0,1 do holdlock[i] = false end
  for i=0,1 do garbage[i] = 0 end
  if (game_mod == 0) then
    for i=0,1 do lines[i] = 0 end
  else
    for i=0,1 do lines[i] = 25 end
  end

  for i=0,1 do cttype[i] = -1 end
  for i=0,1 do nttype[i] = -1 end
  for i=0,1 do hdtype[i] = -1 end
  for i=0,1 do gen_block(i) end
  if (game_2p) new_block(1)
  new_block(0)

  p_mode[0] = 1 --playing
  if (game_2p) p_mode[1] = 1 --playing

  for i=0,1 do framesp[i] = getdelay(i) end
end

menu_spd = 0
menu_hrt = false

function menu_start_game()
  for i=0,1 do game_spd[i] = menu_spd end
  if (menu_hrt) then 
    game_spd[0] += 10
    game_spd[1] += 10
  end
  start_game() --start game
end

function update_menus()

  if (menus_first == 0) play_bgm(1) menus_first=1

  if (btn1(4)) then
  
  if (menu_sel < 2) return menu_start_game()
  game_bgm = 1 - game_bgm
  play_bgm(game_bgm)
  end

  if (btn1(3)) menu_sel+=1
  if (btn1(2)) menu_sel-=1

  if (menu_sel < 0) menu_sel = 0
  if (menu_sel > 2) menu_sel = 2
  
  if (menu_sel > 0) then game_2p = true else game_2p = false end
  
end -- function

function update_clear(ip)

end -- function

function update_game() -- 2 player version
    
local bx = btn1()

for ip=0,1 do
  if (p_mode[ip] == 1) then

    if (cttype[ip] >= 0) then -- moving a block

 				 if (( btn1(4,ip) or btn1(5,ip) ) and ( btn(4,ip) and btn(5,ip)) ) then holdpiece(ip) -- hold piece
      else
        if (btn1(4,ip)) rotate(-1,ip) -- rotate ccw
        if (btn1(5,ip)) rotate(1,ip) -- rotate cw
      end
      if (btnr(0,ip)) move_if_solid(-1,0,ip) -- left
      if (btnr(1,ip)) move_if_solid(1,0,ip) -- right
      if (btn1(2,ip)) hard_drop(ip) --up
      if (btnr(3,ip)) drop_block(ip) --down

      framesp[ip] = framesp[ip] - 1
      if (framesp[ip] <= 0) drop_block(ip)

    else -- gravity, check
	
    if (framesp[ip] <= 0) then
	
	  local r = count(clear_xy[ip+1])
      
	  if (r == 3) then
	  	score[ip] += (combo[ip] * 3)
	  elseif (r > 3) then
	  	score[ip] += (combo[ip] * (r-3) * r)
	  end -- r==3
	  if (r == 6) multi[ip] = 1
	  if (r == 7) multi[ip] = 2
	  if (r == 8) multi[ip] = 3
	  if (r >= 9) drop_multi(ip)
	  
	  local sp = count(clear_xys[ip+1]) -- spark score
	  if (sp > 0) score[ip] += (combo[ip] * sp) + 50
	  
	  if ((r >= 3) or (sp > 0)) combo[ip] += 1

	  for v in all(clear_xys[ip+1]) do -- remove all sparks
	    local x = (v % 5)
		local y = flr((v % 100)/5)
	    mset(2+(15*ip)+x,13-y,mget(31,13-y)) -- rewrite with clean square
		del(clear_xys[ip+1],v)
	  end
			
	  for v in all(clear_xy[ip+1]) do -- remove all cleared cells.
	    local x = (v % 5)
		local y = flr((v % 100)/5)
	    mset(2+(15*ip)+x,13-y,mget(31,13-y)) -- rewrite with clean square
		del(clear_xy[ip+1],v)
	  end
	
      if (gravity(ip)) then framesp[ip] = 2 else check_line(ip) 
	  
	  if (count(clear_xy[ip+1]) == 0) then
	  
        if (game_2p) add_garbage(ip)
	    if (new_block(ip)) framesp[ip] = getdelay(ip)

	  end
	  end --gravity
    else --framesp

    framesp[ip] -= 1
	
    local an = flr(framesp[ip]/2)
	while (an > 7) do an -= 3 end

    for v in all(clear_xy[ip+1]) do -- remove all cleared cells.
	  local x = (v % 5)
	  local y = flr((v % 100)/5)
	  
	  mset(2+(15*ip)+x,13-y,63 - an) -- rewrite with disappearing square
	end
    for v in all(clear_xys[ip+1]) do -- remove all sparks.
	  local x = (v % 5)
	  local y = flr((v % 100)/5)
	  
	  mset(2+(15*ip)+x,13-y,63 - an) -- rewrite with disappearing square
	end

	end --framesp
		
    end --cttype[ip]
  elseif (p_mode[ip] == 2) then --game over
    
	if (framesp[ip] > 0) then 
	  framesp[ip] -= 1
	elseif (btnp(4,ip) or btnp(5,ip)) then
   reset_game()
	end
	
	
  elseif (p_mode[ip] == 3) then --victory
  
  	if (framesp[ip] > 0) then 
	  framesp[ip] -= 1
	elseif (btnp(4,ip) or btnp(5,ip)) then
	  if (game_2p) then
	    reset_game()
	  else
	    mode=3 -- win screen!
	  end
	end
      
  end -- p_mode

end --for ip

end --function

over_mode = 0

function draw_over()
map(16,0,0,0,16,16)
end

function update_over()

  if (over_mode == 0) then
    if (framesleft <= 0) then
      over_mode = 1
      framesleft = 20
    end
    framesleft -= 1
  else
    framesleft -= 1
  end

end -- function

sputnik_frame = 0
dancer_frame = 0

function draw_win ()
map(48,0,0,0,16,16)

if ((sputnik_frame >= 256) and (sputnik_frame <= 512)) then

  if ((sputnik_frame % 32) > 16) then
  spr(198, (512 - sputnik_frame) / 2, 64)
  else
  spr(199, (512 - sputnik_frame) / 2, 64)
  end  
end

local dancer_flip = false
local dancer_spr = 236
if ((dancer_frame % 32) > 16) dancer_flip = true
if ((dancer_frame % 16) > 8) dancer_spr += 2

spr(dancer_spr, 96, 104, 2, 2, dancer_flip)

sputnik_frame = (sputnik_frame + 1) % 512
dancer_frame = (dancer_frame + 1) % 32
end

function update_win()
if ( btnp(4) or btnp(5) ) reset_game()
end

function _update()
btnr_update()
if ( mode == 0) return update_title()
if ( mode == 1) return update_menus()
if ( mode == 2) return update_game()
if ( mode == 3) return update_win()

if ( mode == 15) return update_hiscore()
--update inputs
end
__gfx__
0000000066dd11770000000022222224dd67661d00000000111111115555555522222222111111111111111111111111d2222220111111111111111111111111
000000006dd117760d22222024222242dd67661d0000000011111111555555552222222217717711777117711177771121111110171177177777177177177771
00700700dd1177660200000022422422dd67661d0000000011111111555555552222222217717717717717711177177121111110177177177111177177117711
00077000d117766d0200000022242222dd67661d0000000011111111555555552222222217777717717717711177177121111110177777177777117771117711
00077000117766dd0200000022224222dd67661d0000000011111111555555552222222217717717717717711177177121111110177177177111177177117711
0070070017766dd10200000022422422dd67661d0000000011111111555555552222222217717711777117777177771121111110177177177777177177117711
000000007766dd110200000024222242dd67661d0000000011111111555555552222222211111111111111111111111121111110111111111111111111111111
00000000766dd1170000000042222222dd67661d0000000011111111555555552222222200000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000444444444444444448999842353b3531dcccccd199999994aaaaaaa9
00000000000000000000000000000000000000000000000000000000000000000000000044444444444444448942498253b5b351cd111dc097777774a7777779
0000000000000000000000000000000000000000000000000000000000000000000000004444444444444444942424923b535b31c1ccc1c097999974a7aaaa79
000000000000000000000000000000000000000000000000000000000000000000000000444444442222222292444292b53335b1c1c1c1c097999974a7aaaa79
0000000000000000000000000000000000000000000000000000000000000000000000004444444499999999942424923b535b31c1ccc1c097999974a7aaaa79
00000000000000000000000000000000000000000000000000000000000000000000000044444444202020208942498253b5b351cd111dc097999974a7aaaa79
000000000000000000000000000000000000000000000000000000000000000000000000444444440202020248999842353b3531dcccccd097777774a7777779
00000000000000000000000000000000000000000000000000000000000000000000000044444444222222222222222211111111100000004444444499999999
89843b31ccc19794000000002222222200000000294420110202020200000000000000000000000000000000d6d6dd21d666dd51677766d1766666d7766666d7
9494b3b1c1c177740d222220999499990d222220294220112020202000000000000000000000000000000000651112d060151050748884d066d6d66d676d676d
89843b31ccc19794020000004424442402000000292420110202020200000000000000000000000000000000d16dd1d061010150784d48d06d676d6d66d6d66d
444411111111444402000000424442440200000024442011222222220000000000000000000000000000000061d1d1606510155078ddd8d06676766d6d666d6d
eeeeeeeeeeeeeeee020000002222222202000000294420112222222200000000000000000000000000000000d1ddd1d0d1010150684d48d06d676d6d66d6d66d
eeeeeeeeeeeeeeee020000000000000000000000294220112222222200000000000000000000000000000000d2111560d0151050648884d066d6d66d676d676d
eeeeeeeeeeeeeeee0200001111111111110000002924201122222222000000000000000000000000000000002dd6d6d055555550ddddddd0d66666ddd66666dd
eeeeeeeeeeeeeeee0000001111111111110000002944201122222222000000000000000000000000000000001000000110000001100000017dddddd77dddddd7
0000000000000000000000111111111111000000112944200000000000000000677777766777777667777776d777777dd2222220d2222220d2222220d2222220
00000000000000000200001111111111110d222011294220000000000000000077ccce7777eeeb7777bbbc777766677726d11d60261111602111111021111110
00000000000000000200000022222222000200001129242000000000000000007ccceee77eeebbb77bbbccc7766677772d7667d0216dd6102161161021111110
000000000000000002000020999999940d2200001124442000000000000000007cceeef77eebbbc77bbccce7766777672167761021d77d102117711021177110
00000000000000000200000044244424020000001129442000000000000000007ceeeff77ebbbcc77bcccee7767776672167761021d77d102117711021177110
00000000000000000200000042444244020000001129422000000000000000007eeefff77bbbccc77ccceee7777766672d7667d0216dd6102161161021111110
000000000000000002000000222222220200000011292420000000000000000077efff7777bccc7777ceee777776667726d11d60261111602111111021111110
0000000000000000000000000000000000000000112944200000000000000000677777766777777667777776d777777d00000000000000000000000000000000
4111111111111144411111444444444441111144411111111444444444444444444444444111111111111111144444444111111114444444eee222222222eeee
1777777777777714177777124444444417777712177777777144444444444444444444441777777777777777714444441777777771444444ee22777777722eee
1711111111111714173337124444444417222712171111117714444444444444444444441711111111111111712444441722222277144444e227a92229a722ee
1711111111111712173337124444444417222712171111111771444444444444444444441711111111111111712444441722222227714444e279922e2299a2ee
1711111111111712173337124444444417222712171111111177144444444444444444441711111111111111712444441722222222771444e27992eee27992ee
177777ddd777771217333712444444441722271217ddddddddd77144444441444444444417ddddddddd77777712444441722222222277144e27992eee27992ee
1111171117111112173337114444444417222712171111111111712444441714444444441711111111171111112444441722222222227124e2272222277722ee
4111171117111122173337711444444417222712171111111111712444441771444444441171111111171111112444441722222222227124ee22277777222eee
442217ddd712222417333377114444441722271217dddddddddd712444441777144444444117dddddddd7122224444441722222222227124e2294422222422ee
4444171117124444173333377114444417222712171111111111712444441737714444444411711111111714444444441722222222227124e294422e229442ee
444417ddd712444417333333371144441722271217ddd777dddd71244444173377144444444117dddddddd71444444441722277722227124e27992eee27992ee
4444171117124444173333333371144417222712171172227111712444441733371444444444117111111117144444441722722272227124e27992eee27992ee
444417ddd712444417333333333711441722271217dd722447dd7124411177333711114444444117dddddddd714444441722722447227124e2249922299422ee
444417ddd712444417333333333371241722271217dd724447dd71241777773337777714444444117ddddddd712444441722724447227124ee22444444422eee
4444171117124444173333333333712417222712171172444711712417333333333337124444444117111111712444441722724447227124eee222222222eeee
444417ddd712444417333377333711241722271217dd724447dd712417333333333337124444444417dddddd712444441722724447227124eeee2222222eeeee
444417ddd712444417333371737112241722271217dd724447dd71241733333333333712444444417dddddd71124444417227244472271244444173337124444
444417ccc712444417bbbb71171122441788871217cc724447cc7124177777333777771244444417cccccc71122444441788724447887124444417bbb7124444
444417ddd712444417333371111224441722271217dd724447dd712411111733371111124444417dddddd7112244444417227244472271244444173337124444
444417ddd712444417333371212244441722271217dd724447dd71244111173337111122444417dddddd71122444444417227244472271244444173337124444
444417ccc712444417bbbb71222444441788871217ccc72447cc7124442217333712222444417cccccc71122444444441788872447887124444417bbb7124444
444417ddd712444417333371244444441722271217dddd777ddd712444441733371244444417dddddd7112244444444417222277722271244444173337124444
444417ccc712444417bbbb71244444441788871217cccccccccc71244444173337124444417cccccc7112244444444441788888888887124444417bbb7124444
444417ccc712444417bbbb71244444441788871217cccccccccc7124444417333712444417cccccc71122444444444441788888888887124444417bbb7124444
444417ccc712444417bbbb71244444441788871217cccccccccc7124444417333712444417ccccc711224444444444441788888888887124444417bbb7124444
4444117cc7124444117bbb71244444441178871217cccccccccc7124444417333712444417cccc71122444444444444417888888888871244444117bb7124444
44444117c71244444117bb7124444444411787121177cccccccc7124444417333712444417ccc7112244444444444444117788888888712444444117b7124444
444444117712444444117b71244444444411771241177ccccccc7124444444444444444417cc7112244444444444444441177888888871244444441177124444
44444441171244444441177124444444444117124411777777777124444444444444444417771122444444444444444444117777777771244444444117124444
44444444111244444444111124444444444411124441111111111124444444444444444411111224444444444444444444411111111111244444444411124444
44444444412444444444411244444444444441224444111111111224444444444444444441112244444444444444444444441111111112244444444441244444
44444444444444444444444444444444444444444444422222222244444444444444444444222444444444444444444444444222222222444444444444444444
eeeeeeee11111111111111ee1111111111111111111111111111111111111111111111111111111111111111e111111eeeeeeeeeeeeeeeeeee1111eeeeeeeeee
eeeeeeee17777771177771ee177777711777777117711771177777711777777117777771177777711777777111777711eeeeeeeeeeeeeeeeee1771eeee1111ee
eeeeeeee17711771111771ee111117711111177117711771177111111771111111111771177117711771177117111171e111111eeeeeeeeeee1771eeee1771ee
eeeeeeee17711771ee1771ee1777777117777771177117711777777117777771eeee1771177777711771177117177171e177771eeeeeeeeeee1771eeee1771ee
eeeeeeee17711771ee1771ee1771111111111771177777711111177117711771eeee1771177117711777777117171171e111111eee1111eeee1771eeee1111ee
eeeeeeee17711771ee1771ee1771111111111771111117711111177117711771eeee1771177117711111177117111171eeeeeeeeee1771eeee1111eeee1771ee
eeeeeeee17777771ee1771ee1777777117777771eeee17711777777117777771eeee1771177777711777777111777711eeeeeeeeee1771eeee1771eeee1771ee
eeeeeeee11111111ee1111ee1111111111111111eeee11111111111111111111eeee11111111111111111111e111111eeeeeeeeeee1111eeee1111eeee1111ee
eeeeeeee111111111111111e111111111111111e1111111111111111111111111111111111111111eeee1111111111111111eeee111111111111111111111111
eeeeeeee177777711777771e17777771177777111777777117777771177777711771177117777771eeee1771177117711771eeee177117711771177117777771
eeeeeeee177117711771171e17711111177117711771111117711111177111111771177111177111eeee1771177117711771eeee177777711777177117711771
eeeeeeee17711771177777111771eeee1771177117777771177777711771777117777771ee1771eeeeee1771177777111771eeee177117711771777117711771
eeeeeeee17777771177117711771eeee1771177117711111177111111771177117711771ee1771ee11111771177117711771eeee177117711771177117711771
eeeeeeee17711771177117711771111117711771177111111771eeee177117711771177111177111177117711771177117711111177117711771177117711771
eeeeeeee17711771177777711777777117777771177777711771eeee177777711771177117777771177777711771177117777771177117711771177117777771
eeeeeeee11111111111111111111111111111111111111111111eeee111111111111111111111111111111111111111111111111111111111111111111111111
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111eeee1111777eeeeeeeeeeeeeeeeeeeeeeeeeeeee
1777777117777771177777711777777117777771177117711771177117711771177117711771177117777771ee11177170077eeeeeeeeeeeeeeeeeeeeeeeeeee
1771177117711771177117711771111111177111177117711771177117711771177117711771177111117711e11717717000077eeeeeeeeeeeeeeeeeeeeeeeee
17711771177117711771177117777771ee1771ee1771177117711771177117711177771117711771e117711e1177177170000007eeeeeeeeeeeeeeeeeeeeeeee
17777771177117711771177111111771ee1771ee1771177117711771177117711771177117777771117711ee1777777170000007eeeeeeeeeeeeeeeeeeeeeeee
17711111177177111777771111111771ee1771ee177117711177771117777771177117711117711117711111117711117000077eeeeeeeeeeeeeeeeeeeeeeeee
1771eeee177771711771177117777771ee1771ee17777771e117711e1771177117711771ee1771ee17777771e1171eee70077eeeeeeeeeeeeeeeeeeeeeeeeeee
1111eeee111111111111111111111111ee1111ee11111111ee1111ee1111111111111111ee1111ee11111111ee111eee777eeeeeeeeeeeeeeeeeeeeeeeeeeeee
000000007777777eee777eee7777777e7777777eeeee7e7e7777777eeee7eeee7777777e7777777e7777777eeeeeeeee00000000000000000000000000000000
000000007eeeee7eeeee7eeeeeeeee7eeeeeee7eeee7ee7e7eeeeeeeee7eeeee7eeeee7e7eeeee7e7eeeee7eeeeeeeee00000000000000000000000000000000
000000007eeeee7eeeee7eeeeeeeee7eeeeeee7eee7eee7e7eeeeeeee7eeeeeeeeeee7ee7eeeee7e7eeeee7eeee7eeee00000000000000000000000000000000
000000007eeeee7eeeee7eee7777777ee77777eee7eeee7e7777777e7777777eeeee7eee7777777e7777777eeeeeeeee00000000000000000000000000000000
000000007eeeee7eeeee7eee7eeeeeeeeeeeee7e7eeeee7eeeeeee7e7eeeee7eeee7eeee7eeeee7eeeeee7eeeeeeeeee00000000000000000000000000000000
000000007eeeee7eeeee7eee7eeeeeeeeeeeee7e7777777eeeeeee7e7eeeee7eee7eeeee7eeeee7eeeee7eeeeeeeeeee00000000000000000000000000000000
000000007777777eeeee7eee7777777e777777eeeeeeee7e7777777e7777777ee7eeeeee7777777eeee7eeeeeee7eeee00000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
eee7eeee777777eee77777ee777777ee7777777e7777777ee77777ee7eeeee7ee77777eeeeeeee7e7eeeee7e7eeeeeee7eeeee7e7eeeee7ee77777ee777777ee
ee7e7eee7eeeee7e7eeeee7e7eeeee7e7eeeeeee7eeeeeee7eeeee7e7eeeee7eeee7eeeeeeeeee7e7eeee7ee7eeeeeee77eee77e77eeee7e7eeeee7e7eeeee7e
ee7e7eee7eeeee7e7eeeeeee7eeeee7e7eeeeeee7eeeeeee7eeeeeee7eeeee7eeee7eeeeeeeeee7e7eee7eee7eeeeeee7e7e7e7e7e7eee7e7eeeee7e7eeeee7e
e7eee7ee777777ee7eeeeeee7eeeee7e77777eee77777eee7eee777e7777777eeee7eeeeeeeeee7e7777eeee7eeeeeee7ee7ee7e7ee7ee7e7eeeee7e7eeeee7e
e77777ee7eeeee7e7eeeeeee7eeeee7e7eeeeeee7eeeeeee7eeeee7e7eeeee7eeee7eeeeeeeeee7e7eee7eee7eeeeeee7eeeee7e7eee7e7e7eeeee7e777777ee
7eeeee7e7eeeee7e7eeeee7e7eeeee7e7eeeeeee7eeeeeee7eeeee7e7eeeee7eeee7eeee7eeeee7e7eeee7ee7eeeeeee7eeeee7e7eeee77e7eeeee7e7eeeeeee
7eeeee7e777777eee77777ee777777ee7777777e7eeeeeeee77777ee7eeeee7ee77777eee77777ee7eeeee7e7777777e7eeeee7e7eeeee7ee77777ee7eeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
e77777ee777777eee77777ee7777777e7eeeee7e7eeeee7e7eeeee7e7eeeee7e7eeeee7e7777777eeeeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
7eeeee7e7eeeee7e7eeeee7eeee7eeee7eeeee7e7eeeee7e7eeeee7ee7eee7eee7eee7eeeeeee7eeeeeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
7eeeee7e7eeeee7e7eeeeeeeeee7eeee7eeeee7e7eeeee7e7ee7ee7eee7e7eeeee7e7eeeeeee7eeeeeeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
7eeeee7e777777eee77777eeeee7eeee7eeeee7e7eeeee7e7ee7ee7eeee7eeeeeee7eeeeeee7eeeeeeeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
7eeeee7e7eeee7eeeeeeee7eeee7eeee7eeeee7ee7eee7ee7ee7ee7eee7e7eeeeee7eeeeee7eeeeeeeeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
7eee7e7e7eeeee7e7eeeee7eeee7eeee7eeeee7eee7e7eeee77e77eee7eee7eeeee7eeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
e77777ee7eeeee7ee77777eeeee7eeeee77777eeeee7eeeee7eee7ee7eeeee7eeee7eeee7777777eeee7eeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeee7eeeeeeeeeeeeeeeeeeee7eeeeeeeeeeeee77eeeeeeeeeee7eeeeeeeee7eeeeeeeee7eee7eeeeeeeee77eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeee7eeeeeeeeeeeeeeeeeeee7eeeeeeeeeeee7eeeeeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeee7eeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
ee7777eee77777eeee7777eeee77777eee7777eeee777eeeee77777ee77777eeee77eeeeeeee77eee7eeee7eeeee7eee777777eee77777eeee7777eee77777ee
eeeeee7ee7eeee7ee7eeee7ee7eeee7ee7eeee7eeee7eeeee7eeee7ee7eeee7eeee7eeeeeeeee7eee7eee7eeeeee7eee7ee7ee7ee7eeee7ee7eeee7ee7eeee7e
ee77777ee7eeee7ee7eeeeeee7eeee7ee77777eeeee7eeeee7eeee7ee7eeee7eeee7eeeeeeeee7eee7777eeeeeee7eee7ee7ee7ee7eeee7ee7eeee7ee7eeee7e
e7eeee7ee7eeee7ee7eeee7ee7eeee7ee7eeeeeeeee7eeeeee77777ee7eeee7eeee7eeeeeeeee7eee7eee7eeeeee7eee7ee7ee7ee7eeee7ee7eeee7ee7eeee7e
ee77777ee77777eeee7777eeee77777eee77777eeee7eeeeeeeeee7ee7eeee7eee777eeee7eee7eee7eeee7eeeee7eee7ee7ee7ee7eeee7eee7777eee77777ee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee7777eeeeeeeeeeeeeeeeeeee777eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee7eeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee7777eeee7777eeee7777eeee7777eeee7777eeee7777ee
eeeeeeeeeeeeeeeeeeeeeeeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee7667eeee7667eeee7007eeee7667eee788887ee7cccc7e
ee77777ee7e7777eee77777eee777eeee7eeee7ee7eeee7e7eeeee7ee7eee7eee7eeee7ee77777ee77766777777667777770077777766777788778877c7cc7c7
e7eeee7ee77eeeeee7eeeeeeeee7eeeee7eeee7ee7eeee7e7ee7ee7eee7e7eeee7eeee7eeeee7eee70000667766000077660066776600667787887877cc77cc7
e7eeee7ee7eeeeeeee7777eeeee7eeeee7eeee7ee7eeee7e7ee7ee7eeee7eeeee7eeee7eeee7eeee70000667766000077660066776600667787887877cc77cc7
e7eeee7ee7eeeeeeeeeeee7eeee7eeeee7eeee7eee7ee7ee7ee7ee7eee7e7eeeee77777eee7eeeee77766777777667777776677777700777788778877c7cc7c7
ee77777ee7eeeeeee77777eeeeee77eeee77777eeee77eeee77e77eee7eee7eeeeeeee7ee77777eeee7667eeee7667eeee7667eeee7007eee788887ee7cccc7e
eeeeee7eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee77777eeeeeeeeeeee7777eeee7777eeee7777eeee7777eeee7777eeee7777ee
__label__
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
00000000222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222200000000
0d22222099949999999499999994999999949999999499999994999999949999999499999994999999949999999499999994999999949999999499990d222220
02000000442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442402000000
02000000424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444244424402000000
02000000222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222202000000
02000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
02000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000000
00000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000000
29442011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411294420
29422011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411294220
29242011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411292420
24442011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411244420
29442011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411294420
29422011444444444444444444444444444444444444444444444444444444444444414444444444444444444444444444444444444444444444444411294220
29242011444444444444444444444444444444444444444444444444444444444444171444444444444444444444444444444444444444444444444411292420
29442011444444444444444444444444444444444444444444444444444444444444177144444444444444444444444444444444444444444444444411294420
29442011411111111111114441111144444444444111114441111111144444444444177714444444411111111444444441111111111111111444444411294420
29422011177777777777771417777712444444441777771217777777714444444444173771444444177777777144444417777777777777777144444411294220
29242011171111111111171417333712444444441722271217111111771444444444173377144444172222227714444417111111111111117124444411292420
24442011171111111111171217333712444444441722271217111111177144444444173337144444172222222771444417111111111111117124444411244420
29442011171111111111171217333712444444441722271217111111117714444111773337111144172222222277144417111111111111117124444411294420
29422011177777ddd777771217333712444444441722271217ddddddddd771441777773337777714172222222227714417ddddddddd777777124444411294220
29242011111117111711111217333711444444441722271217111111111171241733333333333712172222222222712417111111111711111124444411292420
29442011411117111711112217333771144444441722271217111111111171241733333333333712172222222222712411711111111711111124444411294420
29442011442217ddd712222417333377114444441722271217dddddddddd7124173333333333371217222222222271244117dddddddd71222244444411294420
29422011444417111712444417333337711444441722271217111111111171241777773337777712172222222222712444117111111117144444444411294220
29242011444417ddd712444417333333371144441722271217ddd777dddd712411111733371111121722277722227124444117dddddddd714444444411292420
24442011444417111712444417333333337114441722271217117222711171244111173337111122172272227222712444441171111111171444444411244420
29442011444417ddd712444417333333333711441722271217dd722447dd71244422173337122224172272244722712444444117dddddddd7144444411294420
29422011444417ddd712444417333333333371241722271217dd724447dd712444441733371244441722724447227124444444117ddddddd7124444411294220
29242011444417111712444417333333333371241722271217117244471171244444173337124444172272444722712444444441171111117124444411292420
29442011444417ddd712444417333377333711241722271217dd724447dd7124444417333712444417227244472271244444444417dddddd7124444411294420
29442011444417ddd712444417333371737112241722271217dd724447dd712444441733371244441722724447227124444444417dd222222222444411294420
29422011444417ccc712444417bbbb71171122441788871217cc724447cc7124444417bbb7124444178872444788712444444417cc2277777772244411294220
29242011444417ddd712444417333371111224441722271217dd724447dd7124444417333712444417227244472271244444417dd227a92229a7224411292420
24442011444417ddd712444417333371212244441722271217dd724447dd712444441733371244441722724447227124444417ddd27992222299a24411244420
29442011444417ccc712444417bbbb71222444441788871217ccc72447cc7124444417bbb7124444178887244788712444417cccc27992224279924411294420
29422011444417ddd712444417333371244444441722271217dddd777ddd7124444417333712444417222277722271244417ddddd27992244279924411294220
29242011444417ccc712444417bbbb71244444441788871217cccccccccc7124444417bbb71244441788888888887124417cccccc22722222777224411292420
29442011444417ccc712444417bbbb71244444441788871217cccccccccc7124444417bbb7124444178888888888712417cccccc712227777722244411294420
29442011444417ccc712444417bbbb71244444441788871217cccccccccc7124444417bbb7124444178888888888712417ccccc7122944222224224411294420
294220114444117cc7124444117bbb71244444441178871217cccccccccc71244444117bb7124444178888888888712417cccc71129442242294424411294220
2924201144444117c71244444117bb7124444444411787121177cccccccc712444444117b7124444117788888888712417ccc711227992444279924411292420
24442011444444117712444444117b71244444444411771241177ccccccc71244444441177124444411778888888712417cc7112227992444279924411244420
29442011444444411712444444411771244444444441171244117777777771244444444117124444441177777777712417771122422499222994224411294420
29422011444444441112444444441111244444444444111244411111111111244444444411124444444111111111112411111224442244444442244411294220
29242011444444444124444444444112444444444444412244441111111112244444444441244444444411111111122441112244444222222222444411292420
29442011444444444444444444444444444444444444444444444222222222444444444444444444444442222222224444222444444422222224444411294420
29442011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411294420
29422011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411294220
29242011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411292420
24442011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411244420
29442011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411294420
29422011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411294220
29242011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411292420
29442011444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444411294420
00000011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111000000
020000111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110d2220
02000000222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222200020000
0200002099999994999999949999999499999994999999949999999499999994999999949999999499999994999999949999999499999994999999940d220000
02000000442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442402000000
02000000424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444244424442444244424402000000
02000000222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222202000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
02020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202
20202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020
02020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222777777777777777777777777777777777777777777777777777777777777222222222222777777777777777777777777777777772222222222222222
22222222700000077000000770000007700000077000000770077007700000077007222222222222700000077000000770077007700000072222222222222222
22222222700770077007700777700777700777777770077770007007700770077007222222222222700777777007700770000007700777772222222222222222
22222222700770077007700722700722700700072270072270070007700770077007222222222222700700077007700770077007700000072222222222222222
22222222700770077007700722700722700770072270072270077007700000077007222222222222700770077000000770077007700777772222222222222222
22222222700770077000007777700777700770077770077770077007700770077007777722222222700770077007700770077007700777772222222222222222
22222222700000077007700770000007700000077000000770077007700770077000000722222222700000077007700770077007700000072222222222222222
22222222777777777777777777777777777777777777777777777777777777777777777722222222777777777777777777777777777777772222222222222222
22222222277777722222222277777777777777777777777777777777222222227777777777777777222222227777777777777772777777777777777722222222
22222222770000772222222270000007700000077000000770000007222222227000000770000007222222227007700770000072700000077000000722222222
22222222707777072222222277777007700770077007700777777007277777727007700770077007222222227000700770077072700777777770077722222222
22222222707007072222222270000007700770077007700722227007270000727007700770077007222222227007000770000077700700072270072222222222
22222222707077072222222270077777700770077007700722227007277777727007700770000007222222227007700770077007700770072270072222222222
22222222707777072222222270077777700770077007700722227007222222227007700777777007222222227007700770077007700770077770077722222222
22222222770000772222222270000007700000077000000722227007222222227000000770000007222222227007700770000007700000077000000722222222
22222222277777722222222277777777777777777777777722227777222222227777777777777777222222227777777777777777777777777777777722222222
22222222777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777772222222222222222222222222
22222222700000077000000770000007700000077000000770000007700000077000000770077007700770077000000770000077222222222222222222222222
22222222700770077007777770077007700770077007700770077777700770077007700770000007700000077007777770077007222222222222222222222222
22222222700770077000000770077007700770077007700770070007700770077007700770077007700770077000000770077007222222222222222222222222
22222222700770077007777770000007700770077007700770077007700770077000000770077007700770077007777770077007222222222222222222222222
22222222700000777007777770077777700000777007700770077007700000777007700770077007700770077007777770077007222222222222222222222222
22222222700770077000000770072222700770077000000770000007700770077007700770077007700770077000000770000007222222222222222222222222
22222222777777777777777777772222777777777777777777777777777777777777777777777777777777777777777777777777222222222222222222222222
22222222277777722222222277777777777777777777772277777777222222227777777777777777777777777777777722222222777777772222222222222222
22222222770000772222222270000007700000077000072270000007222222227000000770000007700000077000000722222222700000072222222222222222
22222222707777072222222277777007700770077770072270077777222222227007700777700777700777777007700727777772700770072222222222222222
22222222707007072222222270000007700770072270072270000007222222227007700722700722700722227007700727000072700000072222222222222222
22222222707077072222222270077777700770072270072270077007222222227000000722700722700722227007700727777772700770072222222222222222
22222222707777072222222270077777700770072270072270077007222222227007777777700777700777777007700722222222700770072222222222222222
22222222770000772222222270000007700000072270072270000007222222227007222270000007700000077000000722222222700000072222222222222222
22222222277777722222222277777777777777772277772277777777222222227777222277777777777777777777777722222222777777772222222222222222
22222222222222222222222277777777777777777777777777777777777777777777777777777777777777777777777722222222222222222222222222222222
22222222222222222222222270000007700000077007700770077007700770077007700770000007700000077007700722222222222222222222222222222222
22222222222222222222222270077777700770077000000770000007700770077000700777700777777007777007700722222222222222222222222222222222
22222222222222222222222270072222700770077007700770077007700770077007000722700722227007227007700722222222222222222222222222222222
22222222222222222222222270072222700770077007700770077007700770077007700722700722227007227000000722222222222222222222222222222222
22222222222222222222222270077777700770077007700770077007700770077007700777700777227007227770077722222222222222222222222222222222
22222222222222222222222270000007700000077007700770077007700000077007700770000007227007222270072222222222222222222222222222222222
22222222222222222222222277777777777777777777777777777777777777777777777777777777227777222277772222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222

__gff__
0000000000000000000000000000000000000000000000000000000305090f0f00000000000000000000000101010f0f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
03090a0b230d0e0f0303030303030303090a0b020d0e0f030000000707070707080808080808080808080808080808080303030303030303030303030303030300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0325070707070735030303030303030325070725070735030000000707070707222323232323232323232323232323240322232323232323232323232323240300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0325070707070735030303030303030325070725070735030000000707070707257b7b7b7b7b7b7b477b7b7b7b7b48350325484848484848484848484848350300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0322232323232324030303030303030322232323232324030000002323232323254041424344454657584c4d494a4b350325484848484848484848484848350300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350303030303030303250c0c0c0c0c35030000000c0c0c0c0c255051525354555667685c5d595a5b350325484848484848484848484848350300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350303030303030303250c0c0c0c0c35030000000c0c0c0c0c25606162636465666e6f6c6d696a6b350325484848484848484848484848350300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350303030303030303250c0c0c0c0c35030000000c0c0c0c0c25707172737475767e7f7c7d797a7b350325484848484848484848484848350300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c022323232323232403250c0c0c0c0c35030000000c0c0c0c0c257b7b7b7b7b7b7b7b7b7b7b7b7b7b350325484848484848484848484848350300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350707070707073503250c0c0c0c0c35030000000c0c0c0c0c323333333333333333333333333333340332333333333333333333333333340300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350707070707073503250c0c0c0c0c35030000000c0c0c0c0c262626262626262626262626262626260303030303030303030303030303030300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350707070707073503250c0c0c0c0c35030000000c0c0c0c0c080808080808080808080808080808082323232323232323232323232323232300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350707070707073503250c0c0c0c0c35030000000c0c0c0c0c080808080808080808080808080808080808080808080808080808080808080800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350707070707073503250c0c0c0c0c35030000000c0c0c0c0c080808080808080808080808080808080808080808080808080808080808080800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03250c0c0c0c0c350707070707073503250c0c0c0c0c35030000000c0c0c0c0c080808080808080808080808080808080808080808080808080808080808080800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0332333333333302333333333333340332333333333334030000003333333333080808080808080808080808080808080808080808080808080808080808080800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0303030303030303030303030303030303030303030303030000000303030303080808080808080808080808080808080808080808080808080808080808080800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
00050000297702306023060227701c7001c7001c7001c7001c7001c7001d5001e6001e6001d5001e4001d5001d5001d5001f3001d400212001d4001d400232001d4001a2001d4001a2001d4001d4000000000000
00020000167701a0701a7700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00040000230501f0001c0401a0401573014050141000000000000000000000000000000001e6001e6001f6001f6001f6000000000000000000000000000000000000000000000000000000000000000000000000
0001000025370000002c37031730317700000034370317302f7702b77027050240300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0005000012070167701c0702407029100267702b07031000320002f0202e070307702f77000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01140000000002b07005430237702307006430000002b070054400000025070054302807024770054302507004430000002177024070044301b030170700c4300000017070054301877000000137700000000000
000e0000300002407022070290002407022070290002a170287702c7702f000253702b7702430026370243002977000000263702a7700000026370000001b7701b7701f770290700000027070000000000000000
00040000300001507016070190701d0701d7701d7701d77031000180702a0002b0002b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010e0000163750d3051b3750d3051e3750f305163750f30519375123052237500305273750f3050c3050f3051637516305143051b37519375183051537516305163751b3051b3050030500305003050030500305
010e0000190700000000000000000000000000000000000020070000000000000000000000000000000000001907019000000000000000000000001a070000001b07000000000000000000000000000000000000
010e00000f070000000f070000000e000000000f070000000f07000000140700000013070000000d0700000014070000000e0700000000000000000e070000000f07000000140700000011070000001207012000
010e00000000000000000000000000000000000000000000000000000000000000001907000000130700000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010e000019375003051b3751b3051e375003050c305123052037500305223750030527375003050c305003051937500305003051b375193751b3051b375003052237500305253050030525375003050030500305
010e00002037500305223750030525375003050030500305253750030527375003052a375003050030500305203750030500305223751e3750030522375003052a375003051d3050030529375003050030500305
010e00000d0700f0700c0700e0700c0700a070080700907012070140701107013070110700f0700d070060700f070080700a0700d0700e07007070090700c0700f07002070080700a0700d070000000f07000000
000e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010e00002a0701b070290701b0702c0701e07025070200702a0701b070200701b070250701907022070190701e0701607019070270702507016070270701607019070000001b070000001b070000000000000000
010e00002400000000270000000000000000000000000000220700000027070000001e070000001b07000000250700000000000200701e0702000020070000002007000000220700000022070000000000000000
010e00001207000000120700000014070000001400000000120700000012070000000f07000000000001c0000f070170000a070000000f0700000000000000000f07000000120700000012070000000000000000
010e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010e00001637500005193051e3751b375273051e3751b305203750000522305223752037500005223752a3052a3750000525305273752c375273052a375293052537500005000050000500005000050000500005
010e00001e0001a000200001d000220001d0001b0001800019000170001b000180001e0001a000190001700019000170001e0001c000200001d0001b00018000190701e0701b07000000180701b0701907000000
000e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000e00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 05004344
01 08090a0b
00 0c0d0e0f
00 10111213
02 14151617
00 45424344

