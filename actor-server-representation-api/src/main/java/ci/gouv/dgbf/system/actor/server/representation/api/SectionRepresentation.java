package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.SectionDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(SectionRepresentation.PATH)
public interface SectionRepresentation extends RepresentationEntity<SectionDto> {
	
	String PATH = "section";
	
}
