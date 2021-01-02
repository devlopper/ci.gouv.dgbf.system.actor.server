package ci.gouv.dgbf.system.actor.server.representation.api;
import javax.ws.rs.Path;

import ci.gouv.dgbf.system.actor.server.representation.entities.AccountingServiceDto;
import org.cyk.utility.server.representation.RepresentationEntity;

@Path(AccountingServiceRepresentation.PATH)
public interface AccountingServiceRepresentation extends RepresentationEntity<AccountingServiceDto> {
	
	String PATH = "accountingservice";
	
}
