package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.CivilityDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(CivilityRepresentation.PATH)
public interface CivilityRepresentation extends RepresentationEntity<CivilityDto> {
	
	String PATH = "civility";
	
}
