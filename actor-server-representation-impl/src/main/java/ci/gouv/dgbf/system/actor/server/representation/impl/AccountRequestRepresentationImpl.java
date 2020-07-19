package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.rest.ResponseBuilder;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.AccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.representation.api.AccountRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestDto;

@ApplicationScoped
public class AccountRequestRepresentationImpl extends AbstractRepresentationEntityImpl<AccountRequestDto> implements AccountRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response accept(Collection<AccountRequestDto> accountRequests) {
		return validate(accountRequests, Boolean.TRUE);
	}

	@Override
	public Response reject(Collection<AccountRequestDto> accountRequests) {
		return validate(accountRequests, Boolean.FALSE);
	}
	
	private Response validate(Collection<AccountRequestDto> accountRequests,Boolean accept) {
		if(CollectionHelper.isEmpty(accountRequests))
			return ResponseBuilder.getInstance().buildRuntimeException(null, "Veuillez fournir les demandes de comptes à "+(Boolean.TRUE.equals(accept) ? "accepter" : "rejeter"));
		Collection<AccountRequest> persistences = __inject__(AccountRequestBusiness.class).findBySystemIdentifiers(accountRequests.stream()
				.map(AccountRequestDto::getIdentifier).collect(Collectors.toList())); 
		Runner.Arguments runnerArguments = new Runner.Arguments().addRunnables(new Runnable() {				
			@Override
			public void run() {
				if(Boolean.TRUE.equals(accept))
					__inject__(AccountRequestBusiness.class).accept(persistences);
				else
					__inject__(AccountRequestBusiness.class).reject(persistences);
			}
		});
		Runner.getInstance().run(runnerArguments);
		if(runnerArguments.getThrowable() == null)
			return Response.ok(persistences.size()+" demende(s) de compte(s) "+(Boolean.TRUE.equals(accept) ? "acceptée(s)" : "rejetée(s)")).build();
		return ResponseBuilder.getInstance().build(runnerArguments.getThrowable());
	}	
}