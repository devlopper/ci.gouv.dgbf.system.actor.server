package ci.gouv.dgbf.system.actor.server.representation.api;
import java.util.Collection;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.identifier.resource.ProxyGetter;
import org.cyk.utility.server.representation.RepresentationEntity;

import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestDto;
import io.swagger.annotations.ApiOperation;

@Path(AccountRequestRepresentation.PATH)
public interface AccountRequestRepresentation extends RepresentationEntity<AccountRequestDto> {

	@POST
	@Path(PATH_RECORD)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Enregistrer des demandes de comptes",tags = {TAG})
	Response record(Collection<AccountRequestDto> accountRequests);
	
	@POST
	@Path(PATH_SUBMIT)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Soumettre des demandes de comptes",tags = {TAG})
	Response submit(Collection<AccountRequestDto> accountRequests);
	
	@POST
	@Path(PATH_NOTIFY_ACCESS_TOKEN_BY_ELECTRONIC_MAIL_ADDRESSES)
	@Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	Response notifyAccessTokenByElectronicMailAddresses(@QueryParam("emails") List<String> electronicMailAddresses);
	
	@POST
	@Path(PATH_ACCEPT)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Accepter des demandes de comptes",tags = {TAG})
	Response accept(Collection<AccountRequestDto> accountRequests);
	
	@POST
	@Path(PATH_REJECT)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@ApiOperation(value = "Rejeter des demandes de comptes",tags = {TAG})
	Response reject(Collection<AccountRequestDto> accountRequests);
	
	static AccountRequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(AccountRequestRepresentation.class);
	}
	
	String PATH = "accountrequest";
	String PATH_RECORD = "record";
	String PATH_SUBMIT = "submit";
	String PATH_NOTIFY_ACCESS_TOKEN_BY_ELECTRONIC_MAIL_ADDRESSES = "notifyAccessTokenByElectronicMailAddresses";	
	String PATH_ACCEPT = "accept";
	String PATH_REJECT = "reject";
	
	String TAG = "demande de compte";
}