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
import org.eclipse.microprofile.openapi.annotations.Operation;

import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestDto;
import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestDto.AccountRequestCreationDto;

@Path(AccountRequestRepresentation.PATH)
public interface AccountRequestRepresentation extends RepresentationEntity<AccountRequestDto> {

	@POST
	@Path(PATH_RECORD)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Enregistrer des demandes de comptes")
	Response record(Collection<AccountRequestDto> accountRequests);
	
	@POST
	@Path(PATH_RECORD_ONE)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Enregistrer une demande de compte")
	Response record(AccountRequestDto accountRequest);
	
	@POST
	@Path(PATH_RECORD_ONE_FOR_EXTERNAL)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Enregistrer une demande de compte")
	Response record(AccountRequestCreationDto accountRequest);
	
	@POST
	@Path(PATH_SUBMIT)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Soumettre des demandes de comptes")
	Response submit(Collection<AccountRequestDto> accountRequests);
	
	@POST
	@Path(PATH_NOTIFY_ACCESS_TOKEN_BY_ELECTRONIC_MAIL_ADDRESSES)
	@Produces({MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML})
	Response notifyAccessTokenByElectronicMailAddresses(@QueryParam("emails") List<String> electronicMailAddresses);
	
	@POST
	@Path(PATH_ACCEPT)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Accepter des demandes de comptes")
	Response accept(Collection<AccountRequestDto> accountRequests);
	
	@POST
	@Path(PATH_REJECT)
	@Consumes({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Produces({ MediaType.APPLICATION_JSON,MediaType.APPLICATION_XML })
	@Operation(description = "Rejeter des demandes de comptes")
	Response reject(Collection<AccountRequestDto> accountRequests);
	
	static AccountRequestRepresentation getProxy() {
		return ProxyGetter.getInstance().get(AccountRequestRepresentation.class);
	}
	
	String PATH = "accountrequest";
	String PATH_RECORD = "record";
	String PATH_RECORD_ONE = "recordone";
	String PATH_RECORD_ONE_FOR_EXTERNAL = "recordoneforexternal";
	String PATH_SUBMIT = "submit";
	String PATH_NOTIFY_ACCESS_TOKEN_BY_ELECTRONIC_MAIL_ADDRESSES = "notifyAccessTokenByElectronicMailAddresses";	
	String PATH_ACCEPT = "accept";
	String PATH_REJECT = "reject";
	
	String TAG = "Demande de compte";
}