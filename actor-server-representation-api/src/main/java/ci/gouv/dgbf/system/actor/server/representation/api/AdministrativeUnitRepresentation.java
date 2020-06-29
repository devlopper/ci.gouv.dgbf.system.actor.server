package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.AdministrativeUnitDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(AdministrativeUnitRepresentation.PATH)
public interface AdministrativeUnitRepresentation extends RepresentationEntity<AdministrativeUnitDto> {
	
	String PATH = "administrativeunit";
	
}
