package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.MenuDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(MenuRepresentation.PATH)
public interface MenuRepresentation extends RepresentationEntity<MenuDto> {
	
	String PATH = "menu";
	
}
