package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestStatusRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestStatusDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class RequestStatusRepresentationImpl extends AbstractRepresentationEntityImpl<RequestStatusDto> implements RequestStatusRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
