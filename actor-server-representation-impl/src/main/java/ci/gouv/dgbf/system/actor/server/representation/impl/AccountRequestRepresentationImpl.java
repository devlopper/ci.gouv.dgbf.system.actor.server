package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.rest.ResponseBuilder;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.AccountRequestBusiness;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AccountRequestQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.representation.api.AccountRequestRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AccountRequestDto;

@ApplicationScoped
public class AccountRequestRepresentationImpl extends AbstractRepresentationEntityImpl<AccountRequestDto> implements AccountRequestRepresentation,Serializable {
	private static final long serialVersionUID = 1L;

	@Override
	public Response record(Collection<AccountRequestDto> accountRequests) {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public Response submit(Collection<AccountRequestDto> accountRequests) {
		// TODO Auto-generated method stub
		return null;
	}
	
	@Override
	public Response notifyAccessTokenByElectronicMailAddresses(List<String> electronicMailAddresses) {
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {			
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						if(CollectionHelper.isEmpty(electronicMailAddresses))
							throw new RuntimeException("Emails obligatoire");
						Collection<AccountRequest> accountRequests = null;
						for(String electronicMailAddress : electronicMailAddresses) {
							AccountRequest accountRequest = AccountRequestQuerier.getInstance().readByElectronicMailAddress(electronicMailAddress);
							if(accountRequest != null) {
								if(accountRequests == null)
									accountRequests = new ArrayList<>();
								accountRequests.add(accountRequest);
							}
						}
						if(CollectionHelper.isEmpty(accountRequests))
							throw new RuntimeException("Aucun de ces emails n'est lié à une demande de compte");
						__inject__(AccountRequestBusiness.class).notifyAccessToken(accountRequests);
					}
				};
			}
		});
	}
	
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