package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;
import javax.ws.rs.core.Response;

import org.cyk.utility.__kernel__.rest.RequestProcessor;
import org.cyk.utility.__kernel__.runnable.Runner;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

import ci.gouv.dgbf.system.actor.server.business.api.IdentityBusiness;
import ci.gouv.dgbf.system.actor.server.representation.api.IdentityRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.IdentityDto;

@ApplicationScoped
public class IdentityRepresentationImpl extends AbstractRepresentationEntityImpl<IdentityDto> implements IdentityRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
	@Override
	public Response encryptElectroncicMailAddress(String electronicMailAddress) {
		Runner.Arguments runnerArguments = new Runner.Arguments();
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runner.Arguments getRunnerArguments() {
				return runnerArguments;
			}
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						runnerArguments.setResult(__inject__(IdentityBusiness.class).encryptElectroncicMailAddress(electronicMailAddress));
					}
				};
			}
		});
	}
	
	@Override
	public Response decryptElectroncicMailAddress(String encryptElectronicMailAddress) {
		Runner.Arguments runnerArguments = new Runner.Arguments();
		return RequestProcessor.getInstance().process(new RequestProcessor.Request.AbstractImpl() {
			@Override
			public Runner.Arguments getRunnerArguments() {
				return runnerArguments;
			}
			@Override
			public Runnable getRunnable() {
				return new Runnable() {					
					@Override
					public void run() {
						runnerArguments.setResult(__inject__(IdentityBusiness.class).decryptElectroncicMailAddress(encryptElectronicMailAddress));
					}
				};
			}
		});
	}
}
