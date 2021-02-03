package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ExpenditureNatureDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ExpenditureNatureRepresentation.PATH)
public interface ExpenditureNatureRepresentation extends RepresentationEntity<ExpenditureNatureDto> {
	
	String PATH = "expenditurenature";
	
}
