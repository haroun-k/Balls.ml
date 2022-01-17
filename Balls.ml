#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Random;;

type coords = {mutable x: float;mutable y: float};;            
type vecteur = {mutable vx:float;mutable vy:float};;
type nature = Player | Spikes | ScoreBall | DoubleScoreBall| VectorBall | RandomBall | EnergyBall | NoEnergyBall | Plateform;; 
type gameState = Menu | Idle | Aiming ;;






type entity = {
        mutable position:coords;
        mutable vitesse:vecteur;
        nature:nature;
        joueur: int;
        get_masse : unit -> float;
        get_rayon : unit -> float;
      set_masse : float -> unit;
      set_rayon : float -> unit} ;;
let map = ({x = 0.; y = 0.;}, {x = 2000.; y = 2000.});;
let time = ref (Unix.gettimeofday () );; 

let generate_cord =  let min, max = map in   {x = float max.x ; y = float max.y };;



let construit_entite pos vit nat r =
    let rayon = ref r in {
            position = pos;
            vitesse = vit;
            nature = nat;
            joueur = if nat = Player then 1 else 0;
            set_masse = (fun m -> rayon := sqrt(m) );
            set_rayon = (fun r -> rayon := r);
            get_masse = (fun() -> !rayon *. !rayon);
            get_rayon = (fun() -> !rayon);
            };;


let player = ref ( construit_entite {x = 1000.; y = 1000.} 	{vx = 0.; vy = 0. }  Player 5. );;
let plateform = ref ( construit_entite {x = 950.; y = 10.}  {vx = 100.; vy = 50.} Plateform 1. );;
let cursor = ref ( {x = 0.; y = 0.} , 0.)


let updateTime() =
	let newTime = Unix.gettimeofday () in 
      let deltaTime = newTime -. !time in
			time := !time+.deltaTime;
	deltaTime;;

			

let data = ref [|!player;!plateform|];;


let highestScore = ref 0 ;;  
(*l'écran d'accueil sur lequel on aura 3 bouttons "start", "howtoplay","quit" et le meilleur score de la session.*)


let getColor entite =
  if entite.nature = Player then (rgb 0 64 128) else 
  if entite.nature = Spikes then (rgb 34 177 76) else 
  if entite.nature = DoubleScoreBall then (rgb 255 255 0) else
  if entite.nature = RandomBall then (rgb 237 109 248) else 
  if entite.nature = VectorBall then (rgb 128 0 128) else (rgb 210 0 0)
;;

let rec drawEntities entites = match entites with
| [] -> ()
| t::queue -> if t.nature = Plateform then 
														fill_rect (int_of_float(t.position.x)) (int_of_float(t.position.y)) (int_of_float(t.vitesse.vy)) (int_of_float(t.vitesse.vy))
												  else 
														let _ = set_color(getColor t) in
														let x= int_of_float( (float_of_int (size_x() ))  *. t.position.x) 
														and y= int_of_float(	(float_of_int (size_y() ))  *. t.position.y) 
														and rx= int_of_float(t.get_rayon()) 
														and ry= int_of_float(t.get_rayon()) in
														(fill_ellipse x y rx ry); drawEntities queue;;


let dotProduct v1 v2 = v1.vx *. v2.vx -.v2.vy *. v1.vy;;
let normOf v = sqrt(dotProduct v v );;
let getAngleFromVectors vect1 vect2 = acos((dotProduct vect1 vect2)/.normOf vect1 *. normOf vect2);;
let createVector point1 point2 =
  {vx = point2.x -. point1.x; vy = point2.y -. point1.y};;

let replaceSpeed player clickT =
  let click,t = clickT in
  let newVector = createVector player.position {x= click.x; y= click.y}in 
    {vx=cos(getAngleFromVectors(player.vitesse) (newVector) )*.normOf newVector;
      vy=sin(getAngleFromVectors(player.vitesse) (newVector) )*.normOf newVector};;




let collision entite1 entite2 = 
  let dist = sqrt( (entite1.position.x*.entite2.position.x) +. (entite1.position.y*.entite2.position.y) ) in
  if dist <= entite1.get_rayon()+.entite2.get_rayon() then true else false;;		
      
let updateSpeed pos speed = 
  let obs =  ref !data.(0) in 
  for i = 1 to Array.length !data do
          if collision !data.(0) !data.(i) then obs := !data.(i) else if !obs != !data.(0) then obs := !obs else obs := !data.(0)
          done;
    if !obs = !data.(1) 
      then {vx = speed.vx; vy = speed.vy -. updateTime()*.0.1} 
      else let theta = getAngleFromVectors {vx=1.;vy=0.}  (createVector  ((!player).position) (!obs).position)  in
      {vx = speed.vx *.  cos(2.*.theta) +. speed.vy *.  sin(2.*.theta);
        vy = speed.vx *.  sin(2.*.theta) +. speed.vy *.  cos(2.*.theta)}
    ;;
    


                            


let updatedPlayerPos (player) = 
  let{ vx; vy }= if !cursor!=({x = 0.; y = 0.} , 0.) then replaceSpeed !player !cursor else updateSpeed (!player).position (!player).vitesse in
  {x=(!player).position.x+.vx; y=(!player).position.y+.vy};;















let game time entities = 
  clear_graph ();
  drawEntities entities;
  let newTime = Unix.gettimeofday () in 
    let deltaTime = newTime -. !time in
    time := !time+.deltaTime;
  let timer = Unix.gettimeofday() in 
    if button_down() then let cx,cy = mouse_pos() in cursor := {x=float_of_int cx;y=float_of_int cy}, timer else if Unix.gettimeofday() -. timer > 0.2 then cursor := ( {x = 0.; y = 0.} , 0.) else  cursor := !cursor ;;

(*Le deuxième écran dans "howtoplay" afin d'expliquer l'effet de chaque boule*)
let htp2 () = 
   clear_graph ();
    (*la couleur du fond d'écran*)
    set_color (rgb 20 20 20);
    fill_rect 0 0 1400 800;
   
   set_color (rgb 229 229 229);
   moveto 350 650 ;
   set_text_size 29;
   draw_string "Voici un résumé des effets de chaque boule";
   
   set_text_size 23;
   set_color (rgb 128 240 240);
   moveto 250 575 ;
   draw_string "Boule Player";
   set_color (rgb 229 229 229);
   moveto 550 575 ;
   draw_string ": C'est vous !";
   
   set_color (rgb 210 0 0);
   moveto 250 525 ;
   draw_string "Boule Score";
   set_color (rgb 229 229 229);
   moveto 550 525 ;
   draw_string ": Chaque boule rapporte 100 de score";
   
   set_color (rgb 255 255 0);
   moveto 250 475 ;
   draw_string "Boule Score Double";
   set_color (rgb 229 229 229);
   moveto 550 475 ;
   draw_string ": C'est vous !";
   
   set_color (rgb 237 109 248);
   moveto 250 425 ;
   draw_string "Boule Random";
   set_color (rgb 229 229 229);
   moveto 550 425 ;
   draw_string ": Cette boule vous propulse dans une direction aléatoire";
   
   set_color (rgb 187 187 187);
   moveto 250 375 ;
   draw_string "Boule Stamina";
   set_color (rgb 229 229 229);
   moveto 550 375 ;
   draw_string ": Cette boule vous remplit la barre d'endurance";
   
   set_color (rgb 246 128 64);
   moveto 250 325 ;
   draw_string "Boule HalfStamina";
   set_color (rgb 229 229 229);
   moveto 550 325 ;
   draw_string ": Cette boule vous retire la moitié de la barre d'endurance ";
   
   set_color (rgb 34 177 76);
   moveto 250 275 ;
   draw_string "Boule Danger";
   set_color (rgb 229 229 229);
   moveto 550 275 ;
   draw_string ": Comme son nom l'indique, elle est dangereuse";
   
   
   set_color (rgb 229 229 229);
   set_text_size 15;
   moveto 550 25;
   draw_string "(appuyer n'importe où pour continuer)";
   wait_next_event [Button_up];
   menu(!highestScore);

(*Le premier écran dans "howtoplay" afin d'expliquer comment jouer au jeu*)
and htp () = 
   clear_graph ();
    (*la couleur du fond d'écran*)
    set_color (rgb 20 20 20);
    fill_rect 0 0 1400 800;
   
   set_color (rgb 229 229 229);
   moveto 500 650 ;
    set_text_size 50;
   draw_string "Comment jouer?";
   
   set_text_size 29;
   moveto 350 550 ;
   draw_string "Vous allez apparaître sur une plateforme.";
   moveto 25 475 ;
   draw_string "Le but est de faire rebondir votre boule sur d'autres afin d'accumuler du score";
   moveto 100 400 ;
   draw_string "Pour ce faire, maintenez le clic de votre souris vers une direction";
   moveto 300 350 ;
   draw_string "afin d'envoyer la balle dans la direction opposée.";
   moveto 100 275;
   draw_string "Attention à ne pas vider la barre d'endurance ou tomber dans la lave !";
   
   set_color (rgb 229 229 229);
   set_text_size 15;
   moveto 550 25;
   draw_string "(appuyer n'importe où pour continuer)";
   wait_next_event [Button_up];
   htp2();


and menu score = 
  if score > !highestScore then highestScore:=score;
  (*Trace simplement le texte et les éléments du menu*)    
  clear_graph ();
  set_color (rgb 20 20 20);
  fill_rect 0 0 1400 800;
  set_color (rgb 224 73 55);

  draw_rect 590 345 200 40;
  draw_rect 590 295 200 40; 
  draw_rect 590 245 200 40; 

  moveto 550 450 ;
  set_text_size 90;
  draw_string  "BALLS" ;
  set_text_size 30;
  
  set_color (rgb 245 194 188);
  moveto 645 350 ;
  draw_string "START" ;
  moveto 630 300 ;
  draw_string "INSTRUCTIONS" ;
  moveto 645 250 ;
  draw_string "LEAVE";
  let waitForClick =  
    let valid = ref false in 
      while not !valid do
        let clic = wait_next_event [Button_up] in
        if 590 < clic.mouse_x && clic.mouse_x < 790 && 345 < clic.mouse_y  && clic.mouse_y  < 385 then game(time entities) else valid := false ;
          if 590 < clic.mouse_x && clic.mouse_x < 790 && 295 < clic.mouse_y  && clic.mouse_y  < 335 then htp() else valid := false ;
          if 590 < clic.mouse_x && clic.mouse_x < 790 && 245 < clic.mouse_y  && clic.mouse_y  < 285 then htp() else valid := false ;
      done; in     
  
  waitForClick();
  ;;



  let complete_entites entites n rayonMax =
  let rec ajoute_entites liste k =
    if k <= 0
      then liste
      else
        let rayon = rayonMax*.0.1 +. Random.float (rayonMax*.0.9) in
          let x = rayon +. (Random.float (1.-. 2. *. rayon))
          and y = rayon +. (Random.float (1.-. 2. *. rayon)) in
            let nouvelleEntite = construit_entite {x=x; y=y} {vx=0.;vy=0.} (if Random.int 2 = 1 then ScoreBall else Spikes) 0 rayon in
              if not (List.exists (collision nouvelleEntite) liste) then
              ajoute_entites (nouvelleEntite ::liste) (k-1)
              else	
              ajoute_entites liste k
              in ajoute_entites entites (n- List.length entites);;






		
let open_window () =   
  open_graph "1400x800";
  set_window_title "Balls?";
  display_mode false;
  auto_synchronize true;;
open_window();;  



menu();;
