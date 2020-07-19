package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestDto;
import io.swagger.annotations.ApiOperation;

@Path(AccountRequestRepresentation.PATH)
public interface AccountRequestRepresentation extends RepresentationEntity<AccountRequestDto> {
	
	@POST
	@Path(PATH_ACCEPT)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Accepter les demandes de comptes",tags = {TAG})
	Response accept(Collection<AccountRequestDto> accountRequests);
	
	@POST
	@Path(PATH_REJECT)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Rejeter les demandes de comptes",tags = {TAG})
	Response reject(Collection<AccountRequestDto> accountRequests);
	
	static AccountRequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(AccountRequestRepresentation.class);
	}
	
	String PATH = "accountrequest";
	String PATH_ACCEPT = "accept";
	String PATH_REJECT = "reject";
	
	String TAG = "demande de compte";
}