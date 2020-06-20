package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.ScopeTypeRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.ScopeTypeDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class ScopeTypeRepresentationImpl extends AbstractRepresentationEntityImpl<ScopeTypeDto> implements ScopeTypeRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
