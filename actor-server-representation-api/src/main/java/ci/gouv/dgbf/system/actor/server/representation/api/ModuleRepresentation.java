package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.ModuleDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(ModuleRepresentation.PATH)
public interface ModuleRepresentation extends RepresentationEntity<ModuleDto> {
	
	String PATH = "module";
	
}
