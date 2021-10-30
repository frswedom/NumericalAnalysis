        subroutine v0fit(itracka_p,itrackb_p,iveecomp_p,cuts,iv_p,idet)
        implicit none
#       include "v0find.inc"
#       include "na49_event_str.gfortran.inc"

        integer itracka_p,itrackb_p,iveecomp_p,iv_p,idet
        real ak0m,alam,prm,pim,pa,pb,pta,ptb,ptot,papar,pbpar
        real eapr,ebpr,eapi,ebpi,ela,ek0,ptarmsq

        type(track_t) tracka,trackb
        type(avertex_v0_t) vee
        type(v0_cuts_vt2_t) cuts(0:2)
        type(vertex_t) svertex

        pointer (iveecomp_p,vee)
        pointer (itracka_p,tracka)
        pointer (itrackb_p,trackb)
        pointer (iv_p,svertex)

        pim = 0.13956755
        prm = 0.93827231
        alam = 1.11563
        ak0m = 0.497671

        vee%px = tracka%px + trackb%px
        vee%py = tracka%py + trackb%py
        vee%pz = tracka%pz + trackb%pz
        vee%sig_px = sqrt( (tracka%sig_px**2) + (trackb%sig_px**2))
        vee%sig_py = sqrt( (tracka%sig_py**2) + (trackb%sig_py**2))
        vee%sig_pz = sqrt( (tracka%sig_pz**2) + (trackb%sig_pz**2))
        vee%pt = sqrt(vee%px**2 + vee%py**2)
        pa = sqrt(tracka%px**2 + tracka%py**2 + tracka%pz**2)
        pb = sqrt(trackb%px**2 + trackb%py**2 + trackb%pz**2)
        pta = sqrt(tracka%px**2 + tracka%py**2)
        ptb = sqrt(trackb%px**2 + trackb%py**2)
        eapr = sqrt(prm**2+ pa**2)
        ebpr = sqrt(prm**2+ pb**2)
        eapi = sqrt(pim**2+ pa**2)
        ebpi = sqrt(pim**2+ pb**2)
        ptot = sqrt(vee%px**2 + vee%py**2 + vee%pz**2)
        ela = sqrt(ptot**2 + alam**2)
        ek0 = sqrt(ptot**2 + ak0m**2)
***
*  Calculating rapidities
***
        vee%yla = log(( ela + abs(vee%pz))/
     >           (sqrt(alam**2 + vee%pt**2)))
        vee%yk0 = log(( ek0 + abs(vee%pz))/
     >           (sqrt(ak0m**2 + vee%pt**2)))

***
*  Calculating invariant masses
***
          vee%mala = sqrt( pim**2 + prm**2 + 2*(eapr*ebpi -
     >   tracka%px*trackb%px - tracka%py*trackb%py -
     >    tracka%pz*trackb%pz))
          vee%malb = sqrt( pim**2 + prm**2 + 2*(eapi*ebpr -
     >    tracka%px*trackb%px - tracka%py*trackb%py -
     >     tracka%pz*trackb%pz))
          vee%mak0 = sqrt( pim**2 + pim**2 + 2*(eapi*ebpi -
     >     tracka%px*trackb%px - tracka%py*trackb%py -
     >     tracka%pz*trackb%pz))

***
* Calculating alpha
***
        papar = ((vee%px*tracka%px) + (vee%py*tracka%py) +
     >     (vee%pz*tracka%pz))/ ptot
        pbpar = ((vee%px*trackb%px) + (vee%py*trackb%py) +
     >    (vee%pz*trackb%pz))/ ptot

          vee%alpha = (papar - pbpar)/(papar + pbpar)
          ptarmsq = pa**2 - papar**2
          if ( ptarmsq .lt. 0.0) then
            vee%ptarm = 999.0
          else
            vee%ptarm = sqrt(ptarmsq)
          endif
***
*   Track v0s back to z of target. Lambdas will come from target
***
           vee%ytarg = svertex%y - ((vee%py/vee%pz)*
     >        (svertex%z - vee%ztarg))
           vee%xtarg = svertex%x - ((vee%px/vee%pz)*
     >        (svertex%z - vee%ztarg))
*          vee%ztarg =  cuts(idet).ztarg

c 999    end
        end
