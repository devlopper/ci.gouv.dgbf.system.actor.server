package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.FinancialControllerServiceDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(FinancialControllerServiceRepresentation.PATH)
public interface FinancialControllerServiceRepresentation extends RepresentationEntity<FinancialControllerServiceDto> {
	
	String PATH = "financialcontrollerservice";
	
}
