package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.RequestScopeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.RequestScopeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class RequestScopeRepresentationImpl extends AbstractRepresentationEntityImpl<RequestScopeDto> implements RequestScopeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
