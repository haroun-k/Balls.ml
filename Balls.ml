#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Random;;

type coords = {mutable x: float;mutable y: float};;            
type vecteur = {mutable vx:float;mutable vy:float};;
type nature = Player | Spikes | ScoreBall | DoubleScoreBall| VectorBall | RandomBall | EnergyBall | NoEnergyBall | Plateform;; 
type states = Menu | Htp1 | Htp2 |  Idle | Aiming ;;






type entity = {
        mutable position:coords;
        mutable vitesse:vecteur;
        nature:nature;
        joueur: int;
        get_masse : unit -> float;
        get_rayon : unit -> float;
      set_masse : float -> unit;
      set_rayon : float -> unit} ;;
let map = ref ({x = 0.; y = 0.;}, {x = 5600.; y = 4000.});;
let lastTime = ref (Unix.gettimeofday () );; 
let deltaTime = ref 0. ;;
let gameState = ref Menu;;
let generate_cord (rayon )=  let min, max = !map in   {x = rayon+. float (max.x -. rayon); y = rayon+. float (max.y -. rayon)};;



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
let data = ref [|!player;!plateform|];;


let updateTime () =
	let newTime = Unix.gettimeofday () in 
  begin 
    deltaTime := newTime -. !lastTime ;
		lastTime := !lastTime +. !deltaTime;
  end;;

			



let score = ref 0;;
let highestScore = ref 0 ;;  
(*l'écran d'accueil sur lequel on aura 3 bouttons "start", "howtoplay","quit" et le meilleur score de la session.*)


let getColor entite =
  if entite.nature = Player then (rgb 0 64 128) else 
  if entite.nature = Plateform then (rgb 65 64 69) else 
  if entite.nature = Spikes then (rgb 34 177 76) else 
  if entite.nature = DoubleScoreBall then (rgb 255 255 0) else
  if entite.nature = RandomBall then (rgb 237 109 248) else 
  if entite.nature = VectorBall then (rgb 128 0 128) else (rgb 210 0 0)
;;



let isInSight (obj) = 
  let leftCorner = {x=(!player).position.x-.float_of_int (size_x())/.2. ; y= (!player).position.y-.	float_of_int (size_y())/.2. } in 
  let rightCorner = {x=(!player).position.x+.float_of_int (size_x())/.2. ; y= (!player).position.y+.	float_of_int (size_y())/.2. } in 
  if leftCorner.x < obj.position.x && obj.position.x < rightCorner.x && leftCorner.y < obj.position.y && obj.position.y <  rightCorner.y then true else false;;

let drawEntities ()=
  let ent = !data in 
    for i = 1 to Array.length ent do
      if isInSight(ent.(i)) then 
                  if (ent.(i)).nature = Plateform then 
                    set_color (getColor ent.(i) );
                    fill_rect (int_of_float  ((ent.(i)).position.x -. (!player).position.x ))   (int_of_float((ent.(i)).position.y -. (!player).position.y)) (int_of_float((ent.(i)).vitesse.vy)) (int_of_float((ent.(i)).vitesse.vy));
                    let _ = set_color(getColor ent.(i) ) in
                    let x= int_of_float(  float_of_int (size_x() )  -. (ent.(i)).position.x) 
                    and y= int_of_float(	float_of_int (size_y() )  -. (ent.(i)).position.y) 
                    and rx= int_of_float((ent.(i)).get_rayon()) 
                    and ry= int_of_float((ent.(i)).get_rayon()) in
                    (fill_ellipse x y rx ry);
          done;;


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
      
let updateSpeed coeff = 
  let obs =  ref !data.(0) in 
  for i = 1 to Array.length !data do
          if collision !data.(0) !data.(i) then obs := !data.(i) else if !obs != !data.(0) then obs := !obs else obs := !data.(0)
          done;
    if !obs = !data.(1) 
      then {vx = (!player).vitesse.vx; vy = (!player).vitesse.vy -. (!deltaTime) *.coeff} 
      else let theta = getAngleFromVectors {vx=1.;vy=0.}  (createVector  ((!player).position) (!obs).position)  in
      {vx = (!player).vitesse.vx *.  cos(2.*.theta) +. (!player).vitesse.vy *.  sin(2.*.theta);
        vy = (!player).vitesse.vx *.  sin(2.*.theta) +. (!player).vitesse.vy *.  cos(2.*.theta)}
    ;;
    


                            


let updatedPlayerPos (coeff) = 
  let{ vx; vy }= if !cursor!=({x = 0.; y = 0.} , 0.) then replaceSpeed !player !cursor else coeff in
  {x=(!player).position.x+.vx; y=(!player).position.y+.vy};;












let drawHowToPlayPage2 () =
  clear_graph ();
   (*la couleur du fond d'écran*)
   set_color (rgb 20 20 20);
   fill_rect 0 0 1400 800;

   set_color (rgb 229 229 229);
   moveto 350 650;
   set_text_size 29;
   draw_string "Voici un résumé des effets de chaque boule";

   set_text_size 23;
   set_color (rgb 128 240 240);
   moveto 250 575;
   draw_string "Player Ball";
   set_color (rgb 229 229 229);
   moveto 550 575;
   draw_string ": C'est vous !";

   set_color (rgb 210 0 0);
   moveto 250 525;
   draw_string "Score Ball";
   set_color (rgb 229 229 229);
   moveto 550 525;
   draw_string ": Chaque boule rapporte 100 de score";

   set_color (rgb 255 255 0);
   moveto 250 475;
   draw_string "DoubleScore Ball";
   set_color (rgb 229 229 229);
   moveto 550 475;
   draw_string ": Chaque boule rapporte le double score";

   set_color (rgb 237 109 248);
   moveto 250 425;
   draw_string "Random Ball";
   set_color (rgb 229 229 229);
   moveto 550 425;
   draw_string ": Cette boule vous propulse dans une direction aléatoire";

   set_color (rgb 187 187 187);
   moveto 250 375;
   draw_string "Stamina Ball";
   set_color (rgb 229 229 229);
   moveto 550 375;
   draw_string ": Cette boule vous remplit la barre d'endurance";

   set_color (rgb 246 128 64);
   moveto 250 325;
   draw_string "HalfStamina Ball";
   set_color (rgb 229 229 229);
   moveto 550 325;
   draw_string ": Cette boule vous retire la moitié de la barre d'endurance ";

   set_color (rgb 34 177 76);
   moveto 250 275;
   draw_string "Danger Ball";
   set_color (rgb 229 229 229);
   moveto 550 275;
   draw_string ": Comme son nom l'indique, elle est dangereuse";
  
  
  set_color (rgb 229 229 229);
  set_text_size 15;
  moveto 550 25;
  draw_string "(appuyer n'importe où pour continuer)";;

let drawHowToPlayPage () = 
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
    draw_string "(appuyer n'importe où pour continuer)";;

let drawHomePage() = 
    if !score > !highestScore then highestScore:=!score;
    (*Trace simplement le texte et les éléments du menu*)
    set_color white;
    moveto 100 20 ;
    draw_string "BEST SCORE :";
    moveto 325 20;
    draw_string (string_of_int !highestScore);

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
    moveto 580 300 ;
    draw_string "INSTRUCTIONS" ;
    moveto 645 250 ;
    draw_string "LEAVE";;
    
    
    
    
    
    
    
    let generate_entites entites n =
          let tab = Array.make n !player in 
          for i = 0 to n do 
              let rayon = 10. in
                let generatedPosition = generate_cord (rayon) in
                let nouvelleEntite = construit_entite {x=generatedPosition.x; y=generatedPosition.y} {vx=0.;vy=0.} (if Random.int 2 = 1 then ScoreBall else Spikes) rayon in
                tab.(i)  <- nouvelleEntite
                done;
          Array.append entites tab;;
    
    (*
    let rec game () = 
      if Array.length !data < 100 then generate_entites !data 10000 else generate_entites !data 0; 
      let timer = Unix.gettimeofday() in 
      if button_down() then 
        let cx,cy = mouse_pos() in cursor := {x=float_of_int cx;y=float_of_int cy}, timer;
      gameState:=Aiming
    else if Unix.gettimeofday() -. timer > 0.1 
      then begin
        cursor := ( {x =0.; y = 0.} , 0.);
        gameState:=Idle;
      end
    else
      cursor := !cursor ;
      clear_graph ();
      drawEntities();
      let _ = updateTime() in 
      if !gameState = Menu then menu() else game();

      
      and menu ()=  
      
      let valid = ref false in 
        while not !valid do
          drawHomePage() ;
          let clic = wait_next_event [Button_up] in
          if !gameState = Menu 
            then 
              drawHomePage();
          if 590 < clic.mouse_x && clic.mouse_x < 790 && 345 < clic.mouse_y  && clic.mouse_y  < 385 then game()  else valid := false ;
          if 590 < clic.mouse_x && clic.mouse_x < 790 && 295 < clic.mouse_y  && clic.mouse_y  < 335 then gameState := Htp1  else valid := false ;
          if 590 < clic.mouse_x && clic.mouse_x < 790 && 245 < clic.mouse_y  && clic.mouse_y  < 285 then close_graph()  else valid := false ;
          if !gameState = Htp1 then
            begin 
              drawHowToPlayPage();
              let _ =  wait_next_event [Button_up] in ();
              drawHowToPlayPage2();
              let _ =  wait_next_event [Button_up] in ();
				      end;
            gameState := Menu;
        done;;
    
        *)

        
        
        
        
        
        
        let open_window () =   
          open_graph "1400x800";
          set_window_title "Balls?";
  display_mode false;
  auto_synchronize true;;
open_window();;


menu();;
