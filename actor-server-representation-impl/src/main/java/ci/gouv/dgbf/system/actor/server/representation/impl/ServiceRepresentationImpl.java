package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ServiceRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ServiceDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ServiceRepresentationImpl extends AbstractRepresentationEntityImpl<ServiceDto> implements ServiceRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
